{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Shelley where

import qualified Codec.CBOR.Read as CBOR (DeserialiseFailure)
import qualified Codec.Serialise as Serialise (decode, encode)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Reflection as Reflection (given)
import Data.Void (Void)
import Network.Socket (SockAddr)

import Cardano.Chain.Slotting (EpochSlots)
import Crypto.Random (drgNew)

import Ouroboros.Byron.Proxy.Block (Block)
import Ouroboros.Consensus.Block (BlockProtocol)
import Ouroboros.Consensus.Protocol (NodeConfig, NodeState)
import Ouroboros.Consensus.Ledger.Byron (ByronGiven)
import qualified Ouroboros.Consensus.Ledger.Byron as Byron
import Ouroboros.Consensus.Ledger.Byron.Config (ByronConfig)
import Ouroboros.Consensus.Util.Condense (Condense (..))
import Ouroboros.Consensus.Util.ThreadRegistry (ThreadRegistry)
import Ouroboros.Storage.ChainDB.API (ChainDB)

import Ouroboros.Network.Block (decodePoint, encodePoint)
import Ouroboros.Network.NodeToNode
import Ouroboros.Network.Mux
import Ouroboros.Network.Protocol.BlockFetch.Codec (codecBlockFetch)
import Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSync)
import Ouroboros.Network.Protocol.Handshake.Version
import Ouroboros.Network.Protocol.TxSubmission.Codec (codecTxSubmission)
import Ouroboros.Network.Protocol.LocalTxSubmission.Codec (codecLocalTxSubmission)
import Ouroboros.Network.Server.ConnectionTable (ConnectionTable, newConnectionTable)
import Ouroboros.Consensus.BlockchainTime (BlockchainTime)
import Ouroboros.Consensus.ChainSyncClient (ClockSkew (..))
import Ouroboros.Consensus.Node
import Ouroboros.Consensus.Node.Tracers (nullTracers)
import Ouroboros.Consensus.Node.Run.Abstract (nodeBlockFetchSize,
           nodeBlockMatchesHeader, nodeDecodeGenTxId, nodeEncodeGenTxId,
           nodeDecodeGenTx, nodeEncodeGenTx)
import Ouroboros.Consensus.NodeNetwork

newtype Peer = Peer { getPeer :: (SockAddr, SockAddr) }
  deriving (Eq, Ord, Show)

instance Condense Peer where
  condense = show

mkPeer :: SockAddr -> SockAddr -> Peer
mkPeer a b = Peer (a, b)

type InitiatorVersions = Versions NodeToNodeVersion DictVersion (OuroborosApplication 'InitiatorApp Peer NodeToNodeProtocols IO Lazy.ByteString () Void)

type ResponderVersions = Versions NodeToNodeVersion DictVersion (AnyResponderApp Peer NodeToNodeProtocols IO Lazy.ByteString)

-- Must have `ByronGiven` because of the constraints on `nodeKernel`
withVersions
  :: ( ByronGiven )
  => ThreadRegistry IO
  -> ChainDB IO (Block ByronConfig)
  -> NodeConfig (BlockProtocol (Block ByronConfig))
  -> NodeState (BlockProtocol (Block ByronConfig))
  -> BlockchainTime IO
  -> (ConnectionTable IO -> InitiatorVersions -> ResponderVersions -> IO t)
  -> IO t
withVersions tr cdb conf state blockchainTime k = do
  ctable <- newConnectionTable
  let params = mkParams tr cdb conf state blockchainTime
  apps <- networkApps params
  let vs = versions apps
  k ctable (initiatorNetworkApplication <$> vs) (responderNetworkApplication <$> vs)

-- | It's in `IO` because `nodeKernel` needs `IO`.
networkApps
  :: ( ByronGiven )
  => NodeParams IO Peer (Block ByronConfig)
  -> IO (NetworkApplication IO Peer Lazy.ByteString Lazy.ByteString Lazy.ByteString Lazy.ByteString Lazy.ByteString ())
networkApps nodeParams = do
  kernel <- nodeKernel nodeParams
  pure $ consensusNetworkApps
    kernel
    nullProtocolTracers
    codecs
    (protocolHandlers nodeParams kernel)

-- | Found in cardano-node that the network magic should be 0.
vData :: NodeToNodeVersionData
vData = NodeToNodeVersionData { networkMagic = 0 }

versions
  :: NetworkApplication IO Peer Lazy.ByteString Lazy.ByteString Lazy.ByteString Lazy.ByteString Lazy.ByteString ()
  -> Versions NodeToNodeVersion DictVersion (NetworkApplication IO Peer Lazy.ByteString Lazy.ByteString Lazy.ByteString Lazy.ByteString Lazy.ByteString ())
versions = simpleSingletonVersions NodeToNodeV_1 vData (DictVersion nodeToNodeCodecCBORTerm)

codecs
  :: ( ByronGiven )
  => ProtocolCodecs (Block ByronConfig) CBOR.DeserialiseFailure IO Lazy.ByteString Lazy.ByteString Lazy.ByteString Lazy.ByteString Lazy.ByteString
codecs = ProtocolCodecs
  { pcChainSyncCodec = codecChainSync
      Byron.encodeByronHeader
      (Byron.decodeByronHeader epochSlots)
      (encodePoint Byron.encodeByronHeaderHash)
      (decodePoint Byron.decodeByronHeaderHash)
  , pcBlockFetchCodec = codecBlockFetch
      Byron.encodeByronBlock
      Byron.encodeByronHeaderHash
      (Byron.decodeByronBlock epochSlots)
      Byron.decodeByronHeaderHash
  , pcTxSubmissionCodec = codecTxSubmission
      nodeEncodeGenTxId
      nodeDecodeGenTxId
      nodeEncodeGenTx
      nodeDecodeGenTx
  , pcLocalChainSyncCodec = codecChainSync
      Byron.encodeByronBlock
      (Byron.decodeByronBlock epochSlots)
      (encodePoint Byron.encodeByronHeaderHash)
      (decodePoint Byron.decodeByronHeaderHash)
  , pcLocalTxSubmissionCodec = codecLocalTxSubmission
      nodeEncodeGenTx
      nodeDecodeGenTx
      -- Copied from cardano-node's choice.
      Serialise.encode
      Serialise.decode
  }
  where
  epochSlots :: EpochSlots
  epochSlots = Reflection.given

mkParams
  :: ( ByronGiven )
  => ThreadRegistry IO
  -> ChainDB IO (Block ByronConfig)
  -> NodeConfig (BlockProtocol (Block ByronConfig))
  -> NodeState (BlockProtocol (Block ByronConfig))
  -> BlockchainTime IO
  -> NodeParams IO Peer (Block ByronConfig)
mkParams tr cdb nconf nstate blockchainTime = NodeParams
  { tracers = nullTracers
  , threadRegistry = tr
  , maxClockSkew = ClockSkew 1
  , cfg = nconf
  , initState = nstate
  , btime = blockchainTime
  , chainDB = cdb
  , callbacks = NodeCallbacks
      { produceDRG = drgNew
      , produceBlock = \_ _ _ _ _ _ -> error "cannot produce blocks, so why do I have to give this?"
      }
  , blockFetchSize = nodeBlockFetchSize
  , blockMatchesHeader = nodeBlockMatchesHeader
  , maxUnackTxs = maxBound
  }
