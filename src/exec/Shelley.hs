{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Shelley where

import qualified Codec.CBOR.Read as CBOR (DeserialiseFailure)
import Control.Tracer (nullTracer)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Reflection as Reflection (given)
import Data.Void (Void)
import Network.Socket (SockAddr)

import Cardano.Chain.Slotting (EpochSlots)
import Crypto.Random (drgNew)

import Ouroboros.Byron.Proxy.Block (Block, decodeBlock, decodeHeader, encodeBlock, encodeHeader)
import Ouroboros.Consensus.Block (BlockProtocol)
import Ouroboros.Consensus.Protocol (NodeConfig, NodeState)
import Ouroboros.Consensus.Ledger.Byron (ByronGiven, decodeByronHeaderHash, encodeByronHeaderHash)
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
import Ouroboros.Consensus.BlockchainTime (BlockchainTime)
import Ouroboros.Consensus.ChainSyncClient (ClockSkew (..))
import Ouroboros.Consensus.Node
import Ouroboros.Consensus.Node.Run.Abstract (nodeBlockFetchSize, nodeBlockMatchesHeader)
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
  -> (InitiatorVersions -> ResponderVersions -> IO t)
  -> IO t
withVersions tr cdb conf state blockchainTime k = do
  let params = mkParams tr cdb conf state blockchainTime
  apps <- networkApps params
  let vs = versions apps
  k (initiatorNetworkApplication <$> vs) (responderNetworkApplication <$> vs)

-- | It's in `IO` because `nodeKernel` needs `IO`.
networkApps
  :: ( ByronGiven )
  => NodeParams IO Peer (Block ByronConfig)
  -> IO (NetworkApplication IO Peer Lazy.ByteString Lazy.ByteString Lazy.ByteString Lazy.ByteString Lazy.ByteString ())
networkApps nodeParams = do
  let epochSlots = Reflection.given
  kernel <- nodeKernel nodeParams
  pure $ consensusNetworkApps
    nullTracer
    nullTracer
    kernel
    (codecs epochSlots)
    (protocolHandlers nodeParams kernel)

-- | Found in cardano-node that the network magic should be 0.
vData :: NodeToNodeVersionData
vData = NodeToNodeVersionData { networkMagic = 0 }

versions
  :: NetworkApplication IO Peer Lazy.ByteString Lazy.ByteString Lazy.ByteString Lazy.ByteString Lazy.ByteString ()
  -> Versions NodeToNodeVersion DictVersion (NetworkApplication IO Peer Lazy.ByteString Lazy.ByteString Lazy.ByteString Lazy.ByteString Lazy.ByteString ())
versions = simpleSingletonVersions NodeToNodeV_1 vData (DictVersion nodeToNodeCodecCBORTerm)

codecs
  :: EpochSlots
  -> ProtocolCodecs (Block ByronConfig) CBOR.DeserialiseFailure IO Lazy.ByteString Lazy.ByteString Lazy.ByteString Lazy.ByteString Lazy.ByteString
codecs epochSlots = ProtocolCodecs
  { pcChainSyncCodec = codecChainSync
      encodeHeader
      (decodeHeader epochSlots)
      (encodePoint encodeByronHeaderHash)
      (decodePoint decodeByronHeaderHash)
  , pcBlockFetchCodec = codecBlockFetch
      encodeBlock
      encodeByronHeaderHash
      (decodeBlock epochSlots)
      decodeByronHeaderHash
  , pcTxSubmissionCodec = error "txSubmissionCodec"
  , pcLocalChainSyncCodec = error "localChainSyncCodec"
  , pcLocalTxSubmissionCodec = error "localTxSubmissionCodec"
  }

mkParams
  :: ( ByronGiven )
  => ThreadRegistry IO
  -> ChainDB IO (Block ByronConfig)
  -> NodeConfig (BlockProtocol (Block ByronConfig))
  -> NodeState (BlockProtocol (Block ByronConfig))
  -> BlockchainTime IO
  -> NodeParams IO Peer (Block ByronConfig)
mkParams tr cdb nconf nstate blockchainTime = NodeParams
  { tracer = nullTracer
  , mempoolTracer = nullTracer
  , decisionTracer = nullTracer
  , fetchClientTracer = nullTracer
  , txInboundTracer = nullTracer
  , txOutboundTracer = nullTracer
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
