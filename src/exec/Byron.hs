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

import Control.Concurrent.STM (STM, atomically, retry)
import Control.Exception (throwIO)
import Control.Monad (forM_)
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

import Ouroboros.Byron.Proxy.Block (Block (..), ByronBlockOrEBB (..),
         coerceHashToLegacy, unByronHeaderOrEBB, headerHash)
import Ouroboros.Byron.Proxy.Main
import Ouroboros.Consensus.Block (getHeader)
import Ouroboros.Consensus.Ledger.Byron (ByronGiven)
import Ouroboros.Network.Block (blockHash)
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.ChainFragment as CF
import Ouroboros.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Storage.ChainDB.API as ChainDB

-- | Download the best available chain from Byron peers and write to the
-- database, over and over again.
--
-- No exception handling is done.
download
  :: forall cfg x .
     ( ByronGiven, Typeable cfg )
  => Tracer IO Text.Builder
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
            let hh = headerHash (getHeader genesisBlock')
                hh' = blockHash genesisBlock'
            () <- case headerHash (getHeader genesisBlock') == blockHash genesisBlock' of
              True  -> do
                traceWith tracer $ mconcat
                  [ "Hashes match "
                  , Text.fromString (show hh)
                  , " "
                  , Text.fromString (show hh')
                  ]
              False -> error $ "mismatch " ++ show hh ++ " " ++ show hh'
            ChainDB.addBlock db genesisBlock'
            pure $ CSL.headerHash genesisBlock
          Just header -> pure $ coerceHashToLegacy (headerHash header)
        -- Pick a peer from the list of announcers at random and download
        -- the chain.
        let (peer, rndGen') = pickRandom rndGen (btPeers bt)
        traceWith tracer $ mconcat
          [ "Attempting to downloading chain with hash "
          , Text.fromString (show tipHash)
          , " from "
          , Text.fromString (show peer)
          ]
        _ <- downloadChain
               bp
               peer
               (CSL.headerHash (btTip bt))
               [tipHash]
               streamer
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
          k blk blk'
          ChainDB.addBlock db blk'
        pure streamer
    , CSL.streamBlocksDone = pure ()
    }

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

-- | Uses the `ChainDB` to announce the new tip-of-chain as promptly as
-- possible, whenever it should change.
announce
  :: ( ByronGiven, Typeable cfg ) -- Needed for HasHeader instance.
  => Maybe Cardano.HeaderHash -- ^ Of block most recently announced.
  -> ChainDB IO (Block cfg)
  -> ByronProxy
  -> IO x
announce mHashOfLatest db bp = do
  (hash, tipHeader) <- atomically $ do
    fragment <- fmap AF.unanchorFragment (ChainDB.getCurrentChain db)
    case CF.head fragment of
      Nothing     -> retry
      Just header ->
        if Just hash == mHashOfLatest
        then retry
        else pure (hash, header)
        where
        hash = headerHash header
  -- Must decode the legacy header from the cardano-ledger header.
  -- TODO alternatively, the type of `ByronProxy.announceChain` could change
  -- to accept a `Header Block`, and it could deal with the recoding. Probably
  -- better that way
  case unByronHeaderOrEBB tipHeader of
    Right hdr -> case CSL.decodeFull (Lazy.fromStrict (Cardano.headerAnnotation hdr)) of
      Left txt -> error "announce: could not decode main header"
      Right (hdr' :: CSL.MainBlockHeader) -> announceChain bp hdr'
    -- We do not announce EBBs.
    Left ebb  -> pure ()
  announce (Just hash) db bp
