{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTSyntax          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE PatternSynonyms     #-}

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}

{-|
Module      : Ouroboros.Byron.Proxy.Main
Description : Definition of the Byron proxy interface and implementation.

The 'ByronProxy' type gives the interface to a Byron network.
'withByronProxy' will create one, allowing the caller to see the tips-of-chains
of its Byron peers, to download one of these chains, and to announce its own
tip-of-chain header to its peers.
-}

module Ouroboros.Byron.Proxy.Main
  ( ByronProxyConfig (..)
  , configFromCSLConfigs
  , ByronProxy (..)
  , withByronProxy
  , BestTip (..)
  ) where

import           Codec.CBOR.Decoding                       (Decoder)
import qualified Codec.CBOR.Write                          as CBOR (toLazyByteString)
import           Control.Arrow                             (first)
import           Control.Concurrent.Async                  (concurrently, race)
import           Control.Concurrent.STM                    (STM, atomically,
                                                            check)
import           Control.Concurrent.STM.TVar               (TVar, modifyTVar',
                                                            newTVarIO, readTVar)
import           Control.Exception                         (Exception, bracket,
                                                            throwIO)
import           Control.Lens                              ((^.))
import           Control.Monad                             (forM, join, when)
import           Control.Monad.Trans.Class                 (lift)
import           Control.Tracer                            (Tracer, traceWith)
import           Data.ByteString                           (ByteString)
import           Data.Conduit                              (ConduitT, await,
                                                            runConduit, yield,
                                                            (.|))
import           Data.Foldable                             (foldlM)
import           Data.List                                 (maximumBy)
import           Data.List.NonEmpty                        (NonEmpty)
import qualified Data.List.NonEmpty                        as NE
import           Data.Map.Strict                           (Map)
import qualified Data.Map.Strict                           as Map
import           Data.Maybe                                (catMaybes,
                                                            mapMaybe,
                                                            fromMaybe)
import           Data.Ord                                  (comparing)
import           Data.Proxy                                (Proxy (..))
import           Data.Tagged                               (Tagged (..),
                                                            tagWith, untag)
import           Data.Text                                 (Text)
import qualified Data.Text.Lazy.Builder                    as Text (Builder)
import           Data.Word                                 (Word32)
import           Data.Time.Units                           (fromMicroseconds)

import qualified Cardano.Binary                            as Binary
import           Cardano.BM.Data.Severity                  (Severity (..))
import           Cardano.Chain.MempoolPayload              (AMempoolPayload(MempoolTx))
import           Cardano.Chain.Slotting                    (EpochSlots)
import qualified Cardano.Chain.UTxO                        as Cardano
import qualified Cardano.Crypto                            as Cardano (AbstractHash (..))

import qualified Pos.Binary.Class                          as CSL (decodeFull,
                                                                   decodeFull',
                                                                   serialize)
import qualified Pos.Chain.Block                           as Byron.Legacy (Block,
                                                                            BlockHeader,
                                                                            HeaderHash,
                                                                            MainBlockHeader,
                                                                            getBlockHeader,
                                                                            headerHash)
import qualified Pos.Chain.Block                           as CSL (BlockConfiguration (..))
import qualified Pos.Chain.Genesis                         as CSL.Genesis
import           Pos.Chain.Ssc                             (MCCommitment (..),
                                                            MCOpening (..),
                                                            MCShares (..),
                                                            MCVssCertificate (..),
                                                            getCertId)
import           Pos.Chain.Txp                             (TxAux (..), TxId,
                                                            TxMsgContents (..))
import           Pos.Chain.Update                          (BlockVersionData,
                                                            UpdateProposal (..),
                                                            UpdateVote (..))
import qualified Pos.Chain.Update                          as CSL (UpdateConfiguration,
                                                                   lastKnownBlockVersion)
import           Pos.Communication                         (NodeId)
import qualified Pos.Configuration                         as CSL (NodeConfiguration (..))
import           Pos.Core                                  (HasDifficulty (difficultyL),
                                                            addressHash,
                                                            getEpochOrSlot)
import           Pos.Core.Chrono                           (NewestFirst (..),
                                                            OldestFirst (..))
import           Pos.Crypto                                (hash)
import qualified Pos.Crypto                                as CSL (AbstractHash (..))
import           Pos.DB.Block                              (GetHashesRangeError (..),
                                                            GetHeadersFromManyToError (..))
import           Pos.DB.Class                              (SerializedBlock)
import           Pos.Diffusion.Full                        (FullDiffusionConfiguration (..),
                                                            diffusionLayerFull)
import           Pos.Infra.Diffusion.Types
import           Pos.Infra.Network.Types                   (NetworkConfig (..))
import           Pos.Logic.Types                           hiding (streamBlocks)
import qualified Pos.Logic.Types                           as Logic
import           Pos.Util.Trace                            (Trace)
import           Pos.Util.Trace.Named                      (LogNamed)
import qualified Pos.Util.Wlog                             as Wlog (Severity)

import           Ouroboros.Byron.Proxy.Block               (ByronBlock,
                                                            Header,
                                                            coerceHashFromLegacy,
                                                            coerceHashToLegacy,
                                                            headerHash,
                                                            toSerializedBlock)
import           Ouroboros.Byron.Proxy.Index.Types         (Index)
import qualified Ouroboros.Byron.Proxy.Index.Types         as Index
import           Ouroboros.Byron.Proxy.Genesis.Convert     (convertEpochSlots)
import           Ouroboros.Consensus.Block                 (getHeader)
import           Ouroboros.Consensus.Byron.Ledger.Mempool  (GenTx(ByronTx),
                                                            fromMempoolPayload)
import           Ouroboros.Consensus.Byron.Ledger.Serialisation
                                                           (encodeByronBlock)
import           Ouroboros.Consensus.Mempool.API           (GenTxId,
                                                            Mempool)
import qualified Ouroboros.Consensus.Mempool.API           as Mempool
import           Ouroboros.Consensus.Mempool.TxSeq         (TicketNo)
import           Ouroboros.Consensus.NodeKernel            (getMempoolReader,
                                                            getMempoolWriter)
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import qualified Ouroboros.Consensus.Util.ResourceRegistry as ResourceRegistry
import           Ouroboros.Network.Block                   (ChainUpdate (..))
import qualified Ouroboros.Network.TxSubmission.Inbound    as Tx.In
import           Ouroboros.Network.TxSubmission.Mempool.Reader
                                                           ( TxSubmissionMempoolReader (..)
                                                           , MempoolSnapshot (..))

import           Ouroboros.Consensus.Storage.ChainDB.API   (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API   as ChainDB
import           Ouroboros.Consensus.Block.RealPoint       (pattern RealPoint
                                                           ,realPointToPoint)

-- | Configuration used to get a 'ByronProxy'.
-- 'configFromGenesis' will make one from a genesis configuration.
data ByronProxyConfig = ByronProxyConfig
  { -- | The Byron Logic layer needs the BlockVersionData. Technically this
    -- is not constant over a whole blockchain: it can be changed by an update.
    -- However, in cardano-byron-proxy it is only used to determine message
    -- size limits on Byron protocols, so as long as header, block, and tx
    -- size limits do not change, it's ok.
    --
    -- To get a BlockVersionData,
    -- Pos.Chain.Genesis.Config.configBlockVersionData can be used on a Byron
    -- genesis configuration.
    bpcAdoptedBVData     :: !BlockVersionData
    -- | Number fo slots per epoch. Assumed to never change.
  , bpcEpochSlots        :: !EpochSlots
    -- | Byron network configuration. To get one, consider using
    -- Pos.Infra.Network.CLI.intNetworkConfigOpts
  , bpcNetworkConfig     :: !(NetworkConfig ())
    -- | Configuration for the Byron Diffusion layer (i.e. the network part).
  , bpcDiffusionConfig   :: !FullDiffusionConfiguration
  }

-- | Make a 'ByronProxyConfig' using cardano-sl configuration types.
-- This will ensure that the proxy is configured in such a way that it is
-- able to communicate with a Byron peer using the same config.
configFromCSLConfigs
  :: CSL.Genesis.Config
  -> CSL.BlockConfiguration
  -> CSL.UpdateConfiguration
  -> CSL.NodeConfiguration
  -> NetworkConfig ()
  -> Word32 -- ^ Batch size for block streaming.
  -> Trace IO (LogNamed (Wlog.Severity, Text))
  -> ByronProxyConfig
configFromCSLConfigs genesisConfig blockConfig updateConfig nodeConfig networkConfig batchSize trace = ByronProxyConfig
  { bpcAdoptedBVData   = CSL.Genesis.configBlockVersionData genesisConfig
  , bpcEpochSlots      = epochSlots
  , bpcNetworkConfig   = networkConfig
  , bpcDiffusionConfig = FullDiffusionConfiguration
      { fdcProtocolMagic          = CSL.Genesis.configProtocolMagic genesisConfig
      , fdcProtocolConstants      = CSL.Genesis.configProtocolConstants genesisConfig
        -- fromIntergal :: Int -> Word
      , fdcRecoveryHeadersMessage = fromIntegral $ CSL.ccRecoveryHeadersMessage blockConfig
      , fdcLastKnownBlockVersion  = CSL.lastKnownBlockVersion updateConfig
      , fdcConvEstablishTimeout   = timeout
        -- fromIntegral :: Int -> Word32
      , fdcStreamWindow           = fromIntegral $ CSL.ccStreamWindow blockConfig
      , fdcBatchSize              = batchSize
      , fdcTrace                  = trace
      }
  }
  where
  epochSlots = convertEpochSlots (CSL.Genesis.configEpochSlots genesisConfig)
  timeout = fromMicroseconds (1000 * fromIntegral (CSL.ccNetworkConnectionTimeout nodeConfig))

-- | Interface presented by the Byron proxy.
data ByronProxy = ByronProxy
  { -- | A transaction which gives the current 'BestTip'.
    -- These are header announcements from the Byron cluster. They don't
    -- come in through a queue because there's no point in dealing with an
    -- earlier announcement when a later one is available.
    --
    -- FIXME should not use the legacy byron block header here.
    -- Logic.postBlockHeader sources this, but it gives legacy BlockHeaders.
    -- We'd need to be able to get a non-legacy header from it.
    -- Could re-code it...
    bestTip       :: STM (Maybe (BestTip Byron.Legacy.BlockHeader))
    -- | Attempt to download the chain at a given header from a given peer.
    -- Those data can be taken from 'bestTip', but of course may no longer be
    -- correct at the time of the call.
    --
    -- FIXME should not use legacy header hash type here.
  , downloadChain :: forall t .
                     NodeId
                  -> Byron.Legacy.HeaderHash   -- of tip to request
                  -> [Byron.Legacy.HeaderHash] -- of checkpoints
                  -> StreamBlocks Byron.Legacy.Block IO t
                  -> IO t
    -- | Make Byron peers aware of this chain. It's expected that they will
    -- request it if it's better than their own, and the download will be
    -- served by a backing ChainDB, so the blocks for this chain should be in
    -- it.
  , announceChain :: Byron.Legacy.MainBlockHeader -> IO ()
  }

taggedKeyValNoOp
  :: Applicative m
  => Proxy tag
  -> (v -> k)
  -> KeyVal (Tagged tag k) v m
taggedKeyValNoOp ptag keyFromValue = KeyVal
  { toKey      = pure . tagWith ptag . keyFromValue
  -- We don't want it.
  , handleInv  = const (pure False)
  -- We don't have it.
  , handleReq  = const (pure Nothing)
  -- We don't care about it and will not relay it.
  , handleData = const (pure False)
  }

-- | Make a `KeyVal` for transactions backed by a `Mempool`.
-- The `TVar` is expected to be kept somewhat in sync with the actual contents
-- of the mempool, which works on monotonically-increasing ticket numbers and
-- has no index based on hash (which Byron requires).
txKeyValFromMempool
  :: Mempool IO ByronBlock TicketNo
  -> TVar (Map TxId TicketNo)
  -> KeyVal (Tagged TxMsgContents TxId) TxMsgContents IO
txKeyValFromMempool mempool txTicketMapVar = KeyVal
    { toKey = pure . tagWith ptag . keyFromValue
    -- Peer offers this transaction. Give True if we don't have it (we want it)
    -- and False otherwise.
    , handleInv = fmap (maybe True (const False)) . lookupTx
    -- Peer requests this transaction. Give Nothing if we don't have it, and
    -- Just if we do.
    , handleReq = lookupTx
    -- Peer gave us this transaction. Put it into the pool and give False to
    -- mean it should _not_ be relayed.
    , handleData = \txMsgContents -> do
        let tx   = fromByronTxMsgContents txMsgContents
            txid = Tx.In.txId writer tx
        outcome <- Tx.In.mempoolAddTxs writer [tx]
        case outcome of
          []          -> pure False
          -- Relay if it was added.
          (txid' : _) -> pure (txid == txid')
    }
  where
    ptag :: Proxy TxMsgContents
    ptag = Proxy
    keyFromValue :: TxMsgContents -> TxId
    keyFromValue = hash . taTx . getTxMsgContents
    reader :: TxSubmissionMempoolReader (GenTxId ByronBlock) (GenTx ByronBlock) TicketNo IO
    reader = getMempoolReader mempool
    writer :: Tx.In.TxSubmissionMempoolWriter (GenTxId ByronBlock) (GenTx ByronBlock) TicketNo IO
    writer = getMempoolWriter mempool
    lookupTx :: Tagged TxMsgContents TxId -> IO (Maybe TxMsgContents)
    lookupTx = \txId -> atomically $ do
      snapshot <- mempoolGetSnapshot reader
      ticketMap <- readTVar txTicketMapVar
      let outcome :: Maybe (Cardano.ATxAux ByteString)
          outcome = do
            ticketNo <- Map.lookup (untag txId) ticketMap
            image <- mempoolLookupTx snapshot ticketNo
            fmap snd (pickMoneyTransaction image)
      pure $ fmap toByronTxMsgContents outcome

-- | Try to send every transaction found in the mempool out to the Byron
-- network.
--
-- It's possible that some transactions are missed, perhaps because `sendTx`
-- takes too long a time. But that's ok.
-- It's also possible that the transactions sent by this were actually received
-- from Byron, and relayed by the Byron inv/req/data system. That's also ok:
-- if a peer already has it, it won't ask for it (it will ignore the inv).
sendTxsFromMempool :: Mempool IO ByronBlock TicketNo -> Diffusion IO -> IO x
sendTxsFromMempool mempool diff = go (Mempool.zeroIdx mempool)
  where
    go ticketNo = do
      -- Block until a non-empty list of transactions to send appears.
      txsToSend <- atomically $ do
        snapshot <- Mempool.getSnapshot mempool
        -- We only relay money transactions.
        let txsToSend = pickMoneyTransactions (Mempool.snapshotTxsAfter snapshot ticketNo)
        check (not (null txsToSend))
        pure txsToSend
      -- Left fold to take the TicketNo of the rightmost member.
      -- Mempool claims this will always be increasing.
      ticketNo' <- foldlM folder ticketNo txsToSend
      go ticketNo'

    -- Left fold action over a list of transactions to send: send each one and
    -- keep the highest ticket number.
    folder = \_ ((_txid, txaux), idx) -> do
      -- Result is a Bool indicating whether it was accepted.
      -- We don't care.
      _ <- sendTx diff (toByronTxAux txaux)
      pure idx

-- | Keep a map from Byron transaction ids to ticket numbers in sync with the
-- mempool. Of course there could be some lag depending on the scheduler.
updateMempoolIdMap
  :: forall void .
     Mempool IO ByronBlock TicketNo
  -> TVar (Map TxId TicketNo)
  -> IO void
updateMempoolIdMap mempool tvar = go [] (Mempool.zeroIdx mempool)
  where
    go currentTxs ticketNo = join $ atomically $ do
      snapshot <- Mempool.getSnapshot mempool
      let txs = Mempool.snapshotTxs snapshot
          -- The prefix of the current list not in the new list is the removals.
          removals :: [(GenTx ByronBlock, TicketNo)]
          removals = takeUntilMatch currentTxs txs
          -- Additions are found by using the current ticket number
          additions :: [(GenTx ByronBlock, TicketNo)]
          additions = Mempool.snapshotTxsAfter snapshot ticketNo
      -- Retry if there is no change.
      check (not (null removals && null additions))
      -- Update the map according to removals and additions.
      -- This ought to be the same as a `fromList` on `txs`, but the idea is
      -- that it will be faster this way as the mempool grows. Maybe it's not
      -- worth the trouble though?
      let removalList :: [TxId]
          removalList = fmap (toByronTxId . fst . fst) (pickMoneyTransactions removals)
          additionList :: [(TxId, TicketNo)]
          additionList = fmap (first (toByronTxId . fst)) (pickMoneyTransactions additions)
      modifyTVar' tvar $ \idmap ->
        foldl (flip (uncurry Map.insert)) (foldl (flip Map.delete) idmap removalList) additionList
      let currentTxs' = txs
          ticketNo' = case additions of
            [] -> ticketNo
            _  -> snd (last additions)
      pure $ go currentTxs' ticketNo'

    takeUntilMatch :: Eq b => [(a, b)] -> [(a, b)] -> [(a, b)]
    takeUntilMatch []            ys                         = ys
    takeUntilMatch xs            []                         = xs
    takeUntilMatch ((a, b) : xs) ((_, b') : ys) | b == b'   = []
                                                | otherwise = (a, b) : takeUntilMatch xs ys

-- | The ouroboros-consensus transaction type includes update proposals, votes,
-- and delegation certificates. We are interested only in money transactions.
pickMoneyTransaction :: GenTx ByronBlock -> Maybe (Cardano.TxId, Cardano.ATxAux ByteString)
pickMoneyTransaction gtx = case gtx of
  ByronTx txid txaux -> Just (txid, txaux)
  _                  -> Nothing

-- | Useful when filtering out mempool entries that come with ticket numbers
-- in the second component.
pickMoneyTransactions
  :: [(GenTx ByronBlock, t)]
  -> [((Cardano.TxId, Cardano.ATxAux ByteString), t)]
pickMoneyTransactions = mapMaybe $ \(gtx, t) ->
  fmap (flip (,) t) (pickMoneyTransaction gtx)

toByronTxId :: Cardano.TxId -> TxId
toByronTxId (Cardano.AbstractHash txId) = CSL.AbstractHash txId

toByronTxMsgContents :: Cardano.ATxAux ByteString -> TxMsgContents
toByronTxMsgContents = TxMsgContents . toByronTxAux

fromByronTxMsgContents :: TxMsgContents -> GenTx ByronBlock
fromByronTxMsgContents (TxMsgContents txAux) = fromByronTxAux txAux

-- | The `GenTx ByronBlock` contains a `Cardano.TxAux` with `ByteString`
-- annotation. Conversion proceeds by decoding a Byron `TxAux` from that.
-- It's basically an assertion failure if the decoding fails. It means our
-- codec must be wrong.
toByronTxAux :: Cardano.ATxAux ByteString -> TxAux
toByronTxAux txaux = case (decodedTx, decodedWitness) of
    (Right tx', Right witness) -> TxAux tx' witness
    (Left err, _) -> error $ "toByronTxAux: Tx codec inconsistent " ++ show err
    (_, Left err) -> error $ "toByronTxAux: TxWitness codec inconsistent " ++ show err
  where
    decodedTx      = CSL.decodeFull' (Binary.annotation (Cardano.aTaTx txaux))
    decodedWitness = CSL.decodeFull' (Binary.annotation (Cardano.aTaWitness txaux))

-- | In order to get a `GenTx ByronBlock`, we need `ByteString` annotations on
-- its parts. Only way to get that is to encode parts of the transaction.
fromByronTxAux :: TxAux -> GenTx ByronBlock
fromByronTxAux txAux = case Binary.decodeFullAnnotatedBytes "TxAux" decoder cslBytes of
    Right txAux' -> fromMempoolPayload (MempoolTx txAux')
    Left  err    -> error $ "fromByronTxAux: TxAux codec inconsistent " ++ show err
  where
    cslBytes = CSL.serialize txAux
    decoder :: Decoder s (Cardano.ATxAux Binary.ByteSpan)
    decoder  = Binary.fromCBOR

-- | Information about the best tip from the Byron network.
data BestTip tip = BestTip
  { -- | This tip ...
    btTip   :: !tip
    -- | ... was announced by these peers.
  , btPeers :: !(NonEmpty NodeId)
  }

deriving instance Show tip => Show (BestTip tip)
deriving instance Eq tip => Eq (BestTip tip)

instance Functor BestTip where
  fmap f bt = bt { btTip = f (btTip bt) }

data HeaderComparison where
  -- | Same as in the very same header.
  Same      :: HeaderComparison
  -- | Better as in strictly longer (more difficult).
  Better    :: HeaderComparison
  -- | Better as in shorter or equal length but not equal header.
  NotBetter :: HeaderComparison

-- | Equality is done via the header hash. Adjective refers to the first
-- in relation to the second: first is `Better` than the second.
compareHeaders
  :: Byron.Legacy.BlockHeader
  -> Byron.Legacy.BlockHeader
  -> HeaderComparison
compareHeaders bhl bhr = case Byron.Legacy.headerHash bhl == Byron.Legacy.headerHash bhr of
  True  -> Same
  False -> case (bhl ^. difficultyL) `compare` (bhr ^. difficultyL) of
    GT -> Better
    _  -> NotBetter

-- | Does not keep duplicates out of the set of peers, which is fine given how
-- it's used: we expect a better header to come in later and throw away the
-- previous list. Also we don't expect a peer to announce the same header
-- twice.
updateBestTip :: NodeId -> Byron.Legacy.BlockHeader -> BestTip Byron.Legacy.BlockHeader -> BestTip Byron.Legacy.BlockHeader
updateBestTip peer header bt = case compareHeaders header (btTip bt) of
  NotBetter -> bt
  Same      -> bt { btPeers = NE.cons peer (btPeers bt) }
  Better    -> BestTip { btTip = header, btPeers = peer NE.:| [] }

updateBestTipMaybe :: NodeId -> Byron.Legacy.BlockHeader -> Maybe (BestTip Byron.Legacy.BlockHeader) -> BestTip Byron.Legacy.BlockHeader
updateBestTipMaybe peer header = maybe bt (updateBestTip peer header)
  where
  bt = BestTip { btTip = header, btPeers = peer NE.:| [] }

-- | Stream blocks from a given hash. The starting point in the ChainDB is
-- determined by the Index, which is assumed to follow that ChainDB.
--
-- `ChainDB.streamBlocks` demands a definite endpoint for the stream, which
-- we don't have here. So instead, `ChainDB.newBlockReader` is used.
-- The old cardano-sl API basically assumes there are never forks, so if the
-- `ChainDB` presents one, we'll just end the conduit.
bbsStreamBlocks
  :: ResourceRegistry IO
  -> Index IO (Header ByronBlock)
  -> ChainDB IO ByronBlock
  -> (forall a . Text -> IO a)
  -- ^ If decoding fails.
  -> Byron.Legacy.HeaderHash
  -> (ByronBlock -> IO s) -- ^ Run on every block. Allows to re-use this definition
                          -- to stream SerializedBlock or Byron.Legacy.Block.
  -> Bool -- ^ Set True to yield the block at the start hash.
  -> (ConduitT () s IO () -> IO t)
  -> IO t
bbsStreamBlocks rr idx db _ hh f yieldFirst k = do
  mSlotNo <- Index.lookup idx (coerceHashFromLegacy hh)
  case mSlotNo of
    Nothing -> k (pure ())
    Just slotNo -> bracket acquireReader releaseReader $ \rdr -> do
      -- Must include the block _at_ the start point, so we can't go straight
      -- to the reader API: advancing a reader to a given point means the
      -- block at the point will not come out of the reader.
      let point = (RealPoint slotNo (coerceHashFromLegacy hh))
      mStartBlock <- ChainDB.getBlock db point
      case mStartBlock of
        Nothing -> k (pure ())
        Just startBlock -> do
          -- It's assumed that, since the start point is in the ChainDB, the
          -- reader will actually move forward to it. The only case in which it
          -- wouldn't is due to a fork, and legacy Byron API can't deal with that
          -- anyway...
          _ <- ChainDB.readerForward rdr [realPointToPoint point]
          -- ChainDB docs say we must expect this rollback
          mInstruction <- ChainDB.readerInstruction rdr
          case mInstruction of
            Just (RollBack _) -> k (conduitFrom rdr startBlock yieldFirst)
            -- FIXME what do we do here? Apparently this can't happen.
            -- Would be nice if the types could express that.
            _ -> error "bbsStreamBlocks: ChainDB reader unexpected instruction"
  where

  --acquireReader = ChainDB.deserialiseReader <$> ChainDB.newBlockReader db rr
  acquireReader = ChainDB.newBlockReader db rr
  releaseReader = ChainDB.readerClose

  conduitFrom rdr blk shouldYield = do
    s <- lift $ f blk
    when shouldYield (yield s)
    -- Do not using the blocking instruction, because this conduit needs to
    -- end if it reaches the tip (Nothing).
    mInstruction <- lift $ ChainDB.readerInstruction rdr
    case mInstruction of
      Just (AddBlock blk') -> conduitFrom rdr blk' True
      Just (RollBack _)    -> pure ()
      Nothing              -> pure ()

bbsGetSerializedBlock
  :: EpochSlots
  -> Index IO (Header ByronBlock)
  -> ChainDB IO ByronBlock
  -> Byron.Legacy.HeaderHash
  -> IO (Maybe SerializedBlock)
bbsGetSerializedBlock _ idx db hh = do
  mSlotNo <- Index.lookup idx (coerceHashFromLegacy hh)
  case mSlotNo of
    Nothing -> pure Nothing
    Just slotNo -> (fmap . fmap) toSerializedBlock (ChainDB.getBlock db point)
      where
      point = RealPoint slotNo (coerceHashFromLegacy hh)

bbsGetBlockHeader
  :: EpochSlots
  -> Index IO (Header ByronBlock)
  -> ChainDB IO ByronBlock
  -> (forall a . Text -> IO a)
  -> Byron.Legacy.HeaderHash
  -> IO (Maybe Byron.Legacy.BlockHeader)
bbsGetBlockHeader _ idx db onErr hh = do
  mSlotNo <- Index.lookup idx (coerceHashFromLegacy hh)
  case mSlotNo of
    Nothing -> pure Nothing
    Just slotNo -> do
      mBlk <- ChainDB.getBlock db point
      case mBlk of
        -- No error in this case. The index may just be behind.
        Nothing  -> pure Nothing
        -- The Block must be converted to a Byron.Legacy.Block and then its
        -- header taken.
        Just blk -> case CSL.decodeFull (CBOR.toLazyByteString (encodeByronBlock blk)) of
          Left  err       -> onErr err
          Right legacyBlk -> pure $ Just $ Byron.Legacy.getBlockHeader legacyBlk
      where
      point = RealPoint slotNo (coerceHashFromLegacy hh)

-- TODO we're supposed to give 'Either GetHashesRangeError' but let's
-- fill that in later at the use site.
--
-- Runs the block stream conduit from the first hash until either
-- - the second hash is encountered
-- - the optional limit, or max bound of Word, iterations is reached
-- - the stream ends before either of these
--
-- The resulting list includes both endpoints.
bbsGetHashesRange
  :: ResourceRegistry IO
  -> Index IO (Header ByronBlock)
  -> ChainDB IO ByronBlock
  -> (forall a . Text -> IO a)
  -> Maybe Word
  -> Byron.Legacy.HeaderHash
  -> Byron.Legacy.HeaderHash
  -> IO (Maybe (OldestFirst NonEmpty Byron.Legacy.HeaderHash))
bbsGetHashesRange rr idx db onErr mLimit from to = do
  mFrom <- Index.lookup idx (coerceHashFromLegacy from)
  mTo   <- Index.lookup idx (coerceHashFromLegacy to)
  case (mFrom, mTo) of
    (Just fromSlot, Just toSlot) ->
      bracket (acquireIterator streamFrom streamTo) releaseIterator
        (drainIterator (fromMaybe maxBound mLimit) [])
      where
      streamFrom = ChainDB.StreamFromInclusive fromPoint
      streamTo   = ChainDB.StreamToInclusive   toPoint
      fromPoint = RealPoint fromSlot (coerceHashFromLegacy from)
      toPoint   = RealPoint toSlot   (coerceHashFromLegacy to)
    _ -> pure Nothing
  where

  acquireIterator streamFrom streamTo = do
    outcome <- ChainDB.streamBlocks db rr streamFrom streamTo
    case outcome of
      Left (ChainDB.MissingBlock _point)      -> onErr "FIXME put error message"
      Left (ChainDB.ForkTooOld   _streamFrom) -> onErr "FIXME put error message"
      --Right iterator                          -> pure $ ChainDB.deserialiseIterator iterator
      Right iterator                          -> pure iterator

  releaseIterator = ChainDB.iteratorClose

  drainIterator
    :: Word
    -> [Byron.Legacy.HeaderHash]
    -> ChainDB.Iterator IO ByronBlock ByronBlock
    -> IO (Maybe (OldestFirst NonEmpty Byron.Legacy.HeaderHash))
  drainIterator 0 _   _    = pure Nothing
  drainIterator n acc iter = do
    next <- ChainDB.iteratorNext iter
    case next of
      ChainDB.IteratorExhausted   -> pure Nothing
      ChainDB.IteratorBlockGCed _ -> pure Nothing
      ChainDB.IteratorResult blk  ->
        if hh == to
        then pure (Just (OldestFirst (NE.reverse (hh NE.:| acc))))
        else drainIterator (n-1) (hh : acc) iter
        where
        -- Could use `blockHash`, but that requires `ByronGiven`, a
        -- hacky reflections thing. Easy enough to do it direct.
        hh = coerceHashToLegacy (headerHash (getHeader blk))

-- Find the first checkpoint that's in the database and then stream from
-- there.
--
-- we're supposed to give 'Either GetHeadersFromManyToError' but will fill that
-- in later at the use site.
--
-- How is it done in byron full logic layer? For every checkpoint given, it
-- will check whether it's in the database, filtering out those which are not.
-- Then it takes the newest checkpoint that's in the database, and loads
-- from there to the endpoint (Maybe HeaderHash or the current tip if it's
-- Nothing).
-- We'll do the same thing, even though it's probably not ideal.
--
-- One difference: we demand a tip (no 'Maybe HeaderHash'). We'll fill that
-- in at the logic layer using getTip.
--
-- The checkpoint itself is not included inthe result. That's how cardano-sl
-- does it, so it's the de facto contract.
bbsGetBlockHeaders
  :: EpochSlots
  -> ResourceRegistry IO
  -> Index IO (Header ByronBlock)
  -> ChainDB IO ByronBlock
  -> (forall a . Text -> IO a)
  -> Maybe Word
  -> NonEmpty Byron.Legacy.HeaderHash
  -> Maybe Byron.Legacy.HeaderHash -- ^ Optional endpoint.
  -> IO (Maybe (NewestFirst NonEmpty Byron.Legacy.BlockHeader))
bbsGetBlockHeaders epochSlots rr idx db onErr mLimit checkpoints mTip = do
  knownCheckpoints <- fmap catMaybes $ forM (NE.toList checkpoints) (bbsGetBlockHeader epochSlots idx db onErr)
  let newestCheckpoint = maximumBy (comparing getEpochOrSlot) knownCheckpoints
  case knownCheckpoints of
    []    -> pure Nothing
    -- Now we know `newestCheckpoints` is not _|_ (maximumBy is partial).
    _ : _ ->
      let f = \blk -> case CSL.decodeFull (CBOR.toLazyByteString (encodeByronBlock blk)) of
                Left err          -> onErr err
                Right legacyBlock -> pure $ Byron.Legacy.getBlockHeader legacyBlock
      in  bbsStreamBlocks rr idx db onErr (Byron.Legacy.headerHash newestCheckpoint) f True $ \producer ->
            runConduit (producer .| consumer 0 (fromMaybe maxBound mLimit) [])
  where
  consumer :: Word -- Limit
           -> Word -- Counted so far
           -> [Byron.Legacy.BlockHeader]
           -> ConduitT Byron.Legacy.BlockHeader x IO (Maybe (NewestFirst NonEmpty Byron.Legacy.BlockHeader))
  consumer limit 0 acc = do
    -- Drop the first one.
    _ <- await
    consumer limit 1 acc
  consumer limit n acc =
    if n >= limit
    then pure Nothing
    else do
      next <- await
      case next of
        -- We give Just if we eventually reach the tip.
        Nothing        -> pure Nothing
        Just blkHeader ->
          if maybe False ((==) (Byron.Legacy.headerHash blkHeader)) mTip
          then pure $ Just $ NewestFirst $ blkHeader NE.:| acc
          else consumer limit (n+1) (blkHeader : acc)

-- | See Logic.getLcaMainChain
--
-- Although it's assumed the input list is oldest-first, it's _not_ necessarily
-- a contiguous chain. It _is_ assumed that if an earlier entry is not in
-- the database, then no later entries are (they are a monotonic subset of a
-- chain), but it's _not_ assumed that the next element is the child of the
-- previous.
bbsGetLcaMainChain
  :: EpochSlots
  -> Index IO (Header ByronBlock)
  -> ChainDB IO ByronBlock
  -> (forall a . Text -> IO a)
  -> OldestFirst [] Byron.Legacy.HeaderHash
  -> IO (NewestFirst [] Byron.Legacy.HeaderHash, OldestFirst [] Byron.Legacy.HeaderHash)
bbsGetLcaMainChain epochSlots idx db onErr (OldestFirst otherChain) = go [] otherChain
  where
  go !acc [] = pure (NewestFirst acc, OldestFirst [])
  go !acc (hh:older) = do
    inMain <- bbsGetBlockHeader epochSlots idx db onErr hh
    case inMain of
      Nothing -> pure (NewestFirst acc, OldestFirst (hh:older))
      Just _  -> go (hh:acc) older

data BlockDecodeError where
  MalformedBlock :: !Text -> BlockDecodeError
  deriving (Show, Eq)

instance Exception BlockDecodeError

-- | An exception to throw in case 'Pos.Logic.Types.Logic.getTip' is called
-- when the database is empty.
data EmptyDatabaseError where
  EmptyDatabaseError :: EmptyDatabaseError
  deriving (Show, Eq)

instance Exception EmptyDatabaseError

-- | Bring up a Byron proxy.
--
-- The `DB` given must not be empty. If it is, `getTip` will throw an
-- exception. So be sure to seed the DB with the genesis block.
withByronProxy
  :: Tracer IO (Severity, Text.Builder)
  -> ByronProxyConfig
  -> Index IO (Header ByronBlock)
  -> ChainDB IO ByronBlock
  -> Mempool IO ByronBlock TicketNo
  -> (ByronProxy -> IO t)
  -> IO t
withByronProxy trace bpc idx db mempool k = do

    -- The best announced block header, and the identifiers of every peer which
    -- announced it. `Nothing` whenever there is no known announcement. It will
    -- never go from `Just` to `Nothing`, it only starts as `Nothing`.
    tipsTVar :: TVar (Maybe (BestTip Byron.Legacy.BlockHeader)) <- newTVarIO Nothing

    -- Associates Byron transaction identifiers (their hashes) with Shelley
    -- mempool ticket numbers. Needed because the Byron relay system is
    -- random access by TxId.
    -- Kept in sync by `updateMempoolIdMap`,
    txTicketMapVar :: TVar (Map TxId TicketNo) <- newTVarIO mempty

    let byronProxy :: Diffusion IO -> ByronProxy
        byronProxy diff = ByronProxy
          { bestTip = takeBestTip
          , downloadChain = \peer tipHash checkpointHashes sbK -> do
              streamResult <- streamBlocks diff peer tipHash checkpointHashes sbK
              case streamResult of
                Just t  -> pure t
                -- Nothing means streaming is not supported. Fall back to
                -- batching. This will do one batch then finish the
                -- StreamBlocks callback. That may not give all of the
                -- blocks requested.
                Nothing -> do
                  batchResult <- getBlocks diff peer tipHash checkpointHashes
                  case getOldestFirst batchResult of
                    [] -> streamBlocksDone sbK
                    (b : bs) -> streamBlocksMore sbK (b NE.:| bs) >>= streamBlocksDone
          , announceChain = announceBlockHeader diff
          }

        epochSlots :: EpochSlots
        epochSlots = bpcEpochSlots bpc

        takeBestTip :: STM (Maybe (BestTip Byron.Legacy.BlockHeader))
        takeBestTip = readTVar tipsTVar

        background :: forall x . Diffusion IO -> IO x
        background diff = fmap (\(x, _) -> x) $
          concurrently (sendTxsFromMempool mempool diff)
                       (updateMempoolIdMap mempool txTicketMapVar)

        blockDecodeError :: forall x . Text -> IO x
        blockDecodeError text = throwIO $ MalformedBlock text

        mkLogic = \_diffusion -> Logic
          { -- This is only used to determine the message size limit on requesting
            -- block headers, which is sadly a byte limit on a message which contains
            -- _all_ of the headers. It's basically legacy, we'll be streaming blocks
            -- rather than batching.
            getAdoptedBVData   = pure (bpcAdoptedBVData bpc)
            -- Recovery mode is useless. We don't use it.
          , recoveryInProgress = pure False

            -- When a new block header announcement comes in, we update the best
            -- Byron tip `TVar`.
          , postBlockHeader    = \header peer -> atomically $
              modifyTVar' tipsTVar $ Just . updateBestTipMaybe peer header

          -- Money transactions _are_ added to the mempool and relayed, but
          -- other atomic (non-block) data are not.
          , postTx            = txKeyValFromMempool mempool txTicketMapVar

          -- Update proposals, update votes, and SSC stuff are not added to
          -- the mempool nor relayed.
          , postUpdate        = taggedKeyValNoOp
              (Proxy :: Proxy (UpdateProposal, [UpdateVote]))
              (hash . fst)
          , postVote          = taggedKeyValNoOp
              (Proxy :: Proxy UpdateVote)
              (\uv -> (uvProposalId uv, uvKey uv, uvDecision uv))
          , postSscCommitment = taggedKeyValNoOp
              (Proxy :: Proxy MCCommitment)
              (\(MCCommitment (pk, _, _)) -> addressHash pk)
          , postSscOpening    = taggedKeyValNoOp
              (Proxy :: Proxy MCOpening)
              (\(MCOpening key _) -> key)
          , postSscShares     = taggedKeyValNoOp
              (Proxy :: Proxy MCShares)
              (\(MCShares key _) -> key)
          , postSscVssCert    = taggedKeyValNoOp
              (Proxy :: Proxy MCVssCertificate)
              (\(MCVssCertificate vc) -> getCertId vc)

          -- Do not put the cert into the mempool. Do not relay it (give False).
          , postPskHeavy = \_ -> pure False

          -- Given a bunch of hashes, find LCA with main chain.
          -- With only the immutable, we just need to get the tip and that
          -- will be the LCA if it's in the set of hashes.
          --   tip
          , getLcaMainChain      = bbsGetLcaMainChain epochSlots idx db blockDecodeError

          -- MsgGetHeaders conversation
          , Logic.getBlockHeader = bbsGetBlockHeader epochSlots idx db blockDecodeError
          -- MsgGetHeaders conversation
          , getBlockHeaders      = \mLimit checkpoints mTip -> do
              result <- ResourceRegistry.withRegistry $ \rr ->
                bbsGetBlockHeaders epochSlots rr idx db blockDecodeError mLimit checkpoints mTip
              case result of
                Nothing -> pure $ Left $ GHFBadInput ""
                Just it -> pure $ Right it
          -- MsgGetHeaders conversation
          , getTip               = do
              mTip <- ChainDB.getTipBlock db
              case mTip of
                Nothing -> do
                  traceWith trace (Error, "getTip: empty database")
                  throwIO $ EmptyDatabaseError
                Just blk -> case CSL.decodeFull (CBOR.toLazyByteString (encodeByronBlock blk)) of
                  Left  err       -> do
                    traceWith trace (Error, "getTip: malformed block")
                    throwIO $ MalformedBlock err
                  Right (legacyBlk :: Byron.Legacy.Block) -> pure legacyBlk
          -- GetBlocks conversation
          , getHashesRange       = \mLimit from to -> do
              result <- ResourceRegistry.withRegistry $ \rr ->
                bbsGetHashesRange rr idx db blockDecodeError mLimit from to
              case result of
                Nothing -> pure $ Left $ GHRBadInput ""
                Just it -> pure $ Right it
          -- GetBlocks conversation
          , getSerializedBlock   = bbsGetSerializedBlock epochSlots idx db
          -- StreamBlocks conversation
          --
          -- The conduit given to the continuation must _not_ yield the block
          -- at the given start hash! This is not documented in cardano-sl.
          -- That's done by giving `False` to `bbsStreamBlocks`, which uses
          -- the `ChainDB` `Reader` API to do streaming. It stops at a fork.
          , Logic.streamBlocks   = \hh l -> do
              ResourceRegistry.withRegistry $ \rr ->
                bbsStreamBlocks rr idx db blockDecodeError hh (pure . toSerializedBlock) False l
          }

        networkConfig = bpcNetworkConfig bpc
        fdconf = bpcDiffusionConfig bpc

    diffusionLayerFull fdconf networkConfig Nothing mkLogic $ \diffusionLayer -> do
      runDiffusionLayer diffusionLayer $ do
        outcome <- race (background (diffusion diffusionLayer))
                        (k (byronProxy (diffusion diffusionLayer)))
        case outcome of
          Left  impossible -> impossible
          Right t          -> pure t
