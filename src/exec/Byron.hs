{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

module Byron
  ( download
  , announce
  -- TODO these should be exported from the byron-proxy library
  , recodeBlock
  , recodeBlockOrFail
  ) where

import Control.Concurrent.STM (STM, atomically, check, readTVar, registerDelay, retry)
import Control.Exception (SomeException, SomeAsyncException, catch, fromException, throwIO)
import Control.Monad (when)
import Control.Tracer (Tracer, traceWith)
import qualified Data.ByteString.Lazy as Lazy (fromStrict)
import Data.Foldable (foldlM)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import qualified Data.Text.Lazy.Builder as Text
import System.Random (StdGen, getStdGen, randomR)

import qualified Cardano.Binary as Binary
import qualified Cardano.Chain.Block as Cardano
import qualified Cardano.Chain.Slotting as Cardano

import qualified Pos.Binary.Class as CSL (decodeFull, serialize)
import qualified Pos.Chain.Block as CSL (Block, BlockHeader (..), GenesisBlock,
                                         MainBlockHeader, HeaderHash, headerHash)
import qualified Pos.Infra.Diffusion.Types as CSL

import Ouroboros.Byron.Proxy.Block (ByronBlock, checkpointOffsets,
         coerceHashToLegacy, headerHash)
import Ouroboros.Byron.Proxy.Main
import Ouroboros.Consensus.Block (Header)
import Ouroboros.Consensus.Ledger.Byron (ByronHash(..),
         byronHeaderRaw, mkByronBlock)
import Ouroboros.Consensus.Ledger.Byron.Auxiliary as Cardano
import Ouroboros.Consensus.Protocol.Abstract (SecurityParam)
import Ouroboros.Network.Block (ChainHash (..), Point, pointHash)
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.ChainFragment as CF
import Ouroboros.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Storage.ChainDB.API as ChainDB

-- | Download the best available chain from Byron peers and write to the
-- database, over and over again. It will download the best chain from its
-- Byron peers regardless of whether it has a better one in the database.
--
-- The ByronGiven and Typeable constraints are needed in order to use
-- AF.selectPoints, that's all.
download
  :: forall void .
     Tracer IO Text.Builder
  -> CSL.GenesisBlock -- ^ For use as checkpoint when DB is empty. Also will
                      -- be put into an empty DB.
                      -- Sadly, old Byron net API doesn't give any meaning to an
                      -- empty checkpoint set; it'll just fall over.
  -> Cardano.EpochSlots
  -> SecurityParam
  -> ChainDB IO ByronBlock
  -> ByronProxy
  -> IO void
download tracer genesisBlock epochSlots securityParam db bp = do
    gen <- getStdGen
    mTip <- ChainDB.getTipHeader db
    tipHash <- case mTip of
      Nothing -> do
        traceWith tracer "Seeding database with genesis"
        genesisBlock' :: ByronBlock <- recodeBlockOrFail epochSlots throwIO (Left genesisBlock)
        ChainDB.addBlock db genesisBlock'
        pure $ CSL.headerHash genesisBlock
      Just header -> pure $ coerceHashToLegacy (headerHash header)
    mainLoop gen tipHash

  where

  -- The BestTip always gives the longest chain seen so far by Byron. All we
  -- need to do here is wait until it actually changes, then try to download.
  -- For checkpoints, we just need to choose some good ones up to k blocks
  -- back, and everything should work out fine. NB: the checkpoints will only
  -- be on the main chain.
  -- getCurrentChain will give exactly what we need.
  waitForNext
    :: CSL.HeaderHash
    -> STM (BestTip CSL.BlockHeader)
  waitForNext lastDownloadedHash = do
    mBt <- bestTip bp
    case mBt of
      -- Haven't seen any tips from Byron peers.
      Nothing -> retry
      Just bt ->
          if thisHash == lastDownloadedHash
          then retry
          else pure bt
        where
          thisHash = CSL.headerHash (btTip bt)

  mainLoop :: StdGen -> CSL.HeaderHash -> IO void
  mainLoop rndGen tipHash = do
    -- Wait until the best tip has changed from the last one we saw. That can
    -- mean the header changed and/or the list of peers who announced it
    -- changed.
    bt <- atomically $ waitForNext tipHash
    -- Pick a peer from the list of announcers at random and download
    -- the chain.
    let (peer, rndGen') = pickRandom rndGen (btPeers bt)
        targetHash = CSL.headerHash (btTip bt)
    chain <- atomically $ ChainDB.getCurrentChain db
    traceWith tracer $ mconcat
      [ "Attempting to download chain with hash "
      , Text.fromString (show targetHash)
      , " from "
      , Text.fromString (show peer)
      ]
    -- Try to download the chain, but do not die in case of IOExceptions.
    -- The hash of the last downloaded block is returned, so that on the next
    -- recursive call, that chain won't be downloaded again. If there's an
    -- exception, or if batch downloaded was used, this hash may not be the
    -- hash of the tip of the chain that was to be downloaded.
    tipHash' <- downloadChain
        bp
        peer
        targetHash
        (reverse (checkpoints chain))
        (streamer tipHash)
      `catch`
        exceptionHandler tipHash
    mainLoop rndGen' tipHash'

  checkpoints
    :: AF.AnchoredFragment (Header ByronBlock)
    -> [CSL.HeaderHash]
  checkpoints = mapMaybe pointToHash .
    AF.selectPoints (fmap fromIntegral (checkpointOffsets securityParam))

  pointToHash :: Point (Header ByronBlock) -> Maybe CSL.HeaderHash
  pointToHash pnt = case pointHash pnt of
    GenesisHash                -> Nothing
    BlockHash (ByronHash hash) -> Just $ coerceHashToLegacy hash

  streamer :: CSL.HeaderHash -> CSL.StreamBlocks CSL.Block IO CSL.HeaderHash
  streamer tipHash = CSL.StreamBlocks
    { CSL.streamBlocksMore = \blocks -> do
        -- List comes in newest-to-oldest order.
        let orderedBlocks = NE.toList (NE.reverse blocks)
        -- The blocks are legacy CSL blocks. To put them into the DB, we must
        -- convert them to new cardano-ledger blocks. That's done by
        -- encoding and decoding.
        tipHash' <- foldlM commitBlock tipHash orderedBlocks
        pure (streamer tipHash')
    , CSL.streamBlocksDone = pure tipHash
    }

  commitBlock :: CSL.HeaderHash -> CSL.Block -> IO CSL.HeaderHash
  commitBlock _ blk = do
    blk' <- recodeBlockOrFail epochSlots throwIO blk
    ChainDB.addBlock db blk'
    pure $ CSL.headerHash blk

  -- Catch all sync exceptions from the downloadChain call that uses the
  -- cardano-sl library, so that a failure to download will not kill the
  -- program.
  -- No need to trace it; cardano-sl will do that.
  exceptionHandler :: CSL.HeaderHash -> SomeException -> IO CSL.HeaderHash
  exceptionHandler h ex = case fromException ex :: Maybe SomeAsyncException of
    Nothing -> pure h
    Just _  -> throwIO ex

  pickRandom :: StdGen -> NonEmpty t -> (t, StdGen)
  pickRandom rndGen ne =
    let (idx, rndGen') = randomR (0, NE.length ne - 1) rndGen
    in  (ne NE.!! idx, rndGen')

recodeBlockOrFail
  :: Cardano.EpochSlots
  -> (forall x . Binary.DecoderError -> IO x)
  -> CSL.Block
  -> IO ByronBlock
recodeBlockOrFail epochSlots onErr = either onErr pure . recodeBlock epochSlots

recodeBlock
  :: Cardano.EpochSlots
  -> CSL.Block
  -> Either Binary.DecoderError ByronBlock
recodeBlock epochSlots cslBlk = case Binary.decodeFullAnnotatedBytes "Block" decoder cslBytes of
  Right blk -> Right $ mkByronBlock epochSlots blk
  Left  err -> Left  $ err
  where
  cslBytes = CSL.serialize cslBlk
  decoder = Cardano.fromCBORABlockOrBoundary epochSlots

-- | Uses the `ChainDB` to announce the new tip-of-chain.
-- At most one is announced every 1 second. Otherwise we'd see spamming of
-- announcements when doing a big download. FIXME this still isn't ideal.
-- Announce once per slot like in cardano-sl? Would need some way to know when
-- a slot is starting.
--
-- In cardano-sl, a block is announced whenever it is received, verified, and
-- stored... _unless_ in recovery mode! i.e. precisely when the new block is
-- a continuation of the current chain.
-- Do we wish to / need to stick to that style?
announce
  :: Maybe Cardano.HeaderHash -- ^ Of block most recently announced.
  -> ChainDB IO ByronBlock
  -> ByronProxy
  -> IO x
announce mHashOfLast db bp = do
  -- Wait until the current tip has changed from the last hash seen.
  (hash, tipHeader) <- atomically $ do
    fragment <- fmap AF.unanchorFragment (ChainDB.getCurrentChain db)
    case CF.head fragment of
      Nothing     -> retry
      Just header ->
        if Just hash == mHashOfLast
        then retry
        else pure (hash, header)
        where
        hash = headerHash header
  timeout <- registerDelay 1000000 -- FIXME configurable?
  -- After 1 second, if the tip has not changed, announce it.
  shouldAnnounce <- atomically $ do
    readTVar timeout >>= check
    fragment <- fmap AF.unanchorFragment (ChainDB.getCurrentChain db)
    case CF.head fragment of
      Nothing      -> pure False
      Just header' -> pure (headerHash header' == hash)
  when shouldAnnounce $ case byronHeaderRaw tipHeader of
    -- Must decode the legacy header from the cardano-ledger header.
    -- TODO alternatively, the type of `ByronProxy.announceChain` could change
    -- to accept a `Header Block`, and it could deal with the recoding. Probably
    -- better that way
    Cardano.ABOBBlockHdr hdr -> case CSL.decodeFull (Lazy.fromStrict (Cardano.headerAnnotation hdr)) of
      Left  _ -> error "announce: could not decode main header"
      Right (hdr' :: CSL.MainBlockHeader) -> announceChain bp hdr'
    -- We do not announce EBBs.
    Cardano.ABOBBoundaryHdr _ -> pure ()
  announce (Just hash) db bp
