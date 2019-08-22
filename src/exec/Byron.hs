{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Byron
  ( download
  , announce
  -- TODO these should be exported from the byron-proxy library
  , recodeBlock
  , recodeBlockOrFail
  ) where

import Control.Concurrent.STM (STM, atomically, check, readTVar, registerDelay, retry)
import Control.Exception (IOException, catch, throwIO)
import Control.Monad (forM_, when)
import Control.Tracer (Tracer, traceWith)
import qualified Data.ByteString.Lazy as Lazy (fromStrict)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Lazy.Builder as Text
import Data.Typeable (Typeable)
import System.Random (StdGen, getStdGen, randomR)

import qualified Cardano.Binary as Binary
import qualified Cardano.Chain.Block as Cardano
import qualified Cardano.Chain.Slotting as Cardano

import qualified Pos.Binary.Class as CSL (decodeFull, serialize)
import qualified Pos.Chain.Block as CSL (Block, BlockHeader (..), GenesisBlock,
                                         MainBlockHeader, headerHash)
import qualified Pos.Infra.Diffusion.Types as CSL

import Ouroboros.Byron.Proxy.Block (Block, ByronBlockOrEBB (..),
         coerceHashToLegacy, unByronHeaderOrEBB, headerHash)
import Ouroboros.Byron.Proxy.Main
import Ouroboros.Consensus.Ledger.Byron (ByronGiven)
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.ChainFragment as CF
import Ouroboros.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Storage.ChainDB.API as ChainDB

-- | Download the best available chain from Byron peers and write to the
-- database, over and over again.
--
-- No exception handling is done.
download
  :: Tracer IO Text.Builder
  -> CSL.GenesisBlock -- ^ For use as checkpoint when DB is empty. Also will
                      -- be put into an empty DB.
                      -- Sadly, old Byron net API doesn't give any meaning to an
                      -- empty checkpoint set; it'll just fall over.
  -> Cardano.EpochSlots
  -> ChainDB IO (Block cfg)
  -> ByronProxy
  -> (CSL.Block -> Block cfg -> IO ())
  -> IO x
download tracer genesisBlock epochSlots db bp k = getStdGen >>= mainLoop Nothing

  where

  waitForNext
    :: Maybe (BestTip CSL.BlockHeader)
    -> STM (Either (BestTip CSL.BlockHeader) Atom)
  waitForNext mBt = do
    mBt' <- bestTip bp
    if mBt == mBt'
    -- If recvAtom retries then the whole STM will retry and we'll check again
    -- for the best tip to have changed.
    then fmap Right (recvAtom bp)
    else case mBt' of
        Nothing -> retry
        Just bt -> pure (Left bt)

  mainLoop :: Maybe (BestTip CSL.BlockHeader) -> StdGen -> IO x
  mainLoop mBt rndGen = do
    -- Wait until the best tip has changed from the last one we saw. That can
    -- mean the header changed and/or the list of peers who announced it
    -- changed.
    next <- atomically $ waitForNext mBt
    case next of
      -- TODO we don't get to know from where it was received. Problem? Maybe
      -- not.
      Right atom -> do
        traceWith tracer $ mconcat
          [ "Got atom: "
          , Text.fromString (show atom)
          ]
        mainLoop mBt rndGen
      Left bt -> do
        mTip <- ChainDB.getTipHeader db
        tipHash <- case mTip of
          -- If the DB is empty, we use the genesis hash as our tip, but also
          -- we need to put the genesis block into the database, because the
          -- Byron peer _will not serve it to us_!
          Nothing -> do
            traceWith tracer "Seeding database with genesis"
            genesisBlock' :: Block cfg <- recodeBlockOrFail epochSlots throwIO (Left genesisBlock)
            ChainDB.addBlock db genesisBlock'
            pure $ CSL.headerHash genesisBlock
          Just header -> pure $ coerceHashToLegacy (headerHash header)
        -- Pick a peer from the list of announcers at random and download
        -- the chain.
        let (peer, rndGen') = pickRandom rndGen (btPeers bt)
            remoteTipHash = CSL.headerHash (btTip bt)
        traceWith tracer $ mconcat
          [ "Attempting to download chain with hash "
          , Text.fromString (show remoteTipHash)
          , " from "
          , Text.fromString (show peer)
          ]
        -- Try to download the chain, but do not die in case of IOExceptions.
        _ <- downloadChain
               bp
               peer
               remoteTipHash
               [tipHash]
               streamer
          `catch`
          exceptionHandler
        mainLoop (Just bt) rndGen'

  -- If it ends at an EBB, the EBB will _not_ be written. The tip will be the
  -- parent of the EBB.
  -- This should be OK.
  streamer :: CSL.StreamBlocks CSL.Block IO ()
  streamer = CSL.StreamBlocks
    { CSL.streamBlocksMore = \blocks -> do
        -- List comes in newest-to-oldest order.
        let orderedBlocks = NE.toList (NE.reverse blocks)
        -- The blocks are legacy CSL blocks. To put them into the DB, we must
        -- convert them to new cardano-ledger blocks. That's done by
        -- encoding and decoding.
        forM_ orderedBlocks $ \blk -> do
          blk' <- recodeBlockOrFail epochSlots throwIO blk
          ChainDB.addBlock db blk'
          k blk blk'
        pure streamer
    , CSL.streamBlocksDone = pure ()
    }

  -- No need to trace it; cardano-sl libraries will do that.
  exceptionHandler :: IOException -> IO (Maybe ())
  exceptionHandler _ = pure Nothing

  pickRandom :: StdGen -> NonEmpty t -> (t, StdGen)
  pickRandom rndGen ne =
    let (idx, rndGen') = randomR (0, NE.length ne - 1) rndGen
    in  (ne NE.!! idx, rndGen')

recodeBlockOrFail
  :: Cardano.EpochSlots
  -> (forall x . Binary.DecoderError -> IO x)
  -> CSL.Block
  -> IO (Block cfg)
recodeBlockOrFail epochSlots onErr = either onErr pure . recodeBlock epochSlots

recodeBlock
  :: Cardano.EpochSlots
  -> CSL.Block
  -> Either Binary.DecoderError (Block cfg)
recodeBlock epochSlots cslBlk = case Binary.decodeFullAnnotatedBytes "Block" decoder cslBytes of
  Right blk -> Right $ ByronBlockOrEBB blk
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
  :: ( ByronGiven, Typeable cfg ) -- Needed for HasHeader instance.
  => Maybe Cardano.HeaderHash -- ^ Of block most recently announced.
  -> ChainDB IO (Block cfg)
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
  when shouldAnnounce $ case unByronHeaderOrEBB tipHeader of
    -- Must decode the legacy header from the cardano-ledger header.
    -- TODO alternatively, the type of `ByronProxy.announceChain` could change
    -- to accept a `Header Block`, and it could deal with the recoding. Probably
    -- better that way
    Right hdr -> case CSL.decodeFull (Lazy.fromStrict (Cardano.headerAnnotation hdr)) of
      Left  _ -> error "announce: could not decode main header"
      Right (hdr' :: CSL.MainBlockHeader) -> announceChain bp hdr'
    -- We do not announce EBBs.
    Left  _   -> pure ()
  announce (Just hash) db bp
