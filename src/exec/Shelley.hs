{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Shelley where

import qualified Data.ByteString.Lazy as Lazy
import Data.Void (Void)
import Network.Socket (SockAddr)

-- ToCBOR/FromCBOR UTxOValidationError, for local tx submission codec.
import Cardano.Chain.UTxO.Validation ()
import Crypto.Random (drgNew)

import Ouroboros.Byron.Proxy.Block (Block)
import Ouroboros.Consensus.Block (BlockProtocol)
import Ouroboros.Consensus.Protocol (NodeConfig, NodeState)
import Ouroboros.Consensus.Ledger.Byron (ByronGiven)
import Ouroboros.Consensus.Ledger.Byron.Config (ByronConfig)
import Ouroboros.Consensus.Util.Condense (Condense (..))
import Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import Ouroboros.Storage.ChainDB.API (ChainDB)

import Ouroboros.Network.NodeToNode
import Ouroboros.Network.Mux
import Ouroboros.Network.Protocol.Handshake.Version
import Ouroboros.Network.Server.ConnectionTable (ConnectionTable, newConnectionTable)
import Ouroboros.Consensus.BlockchainTime (BlockchainTime)
import Ouroboros.Consensus.ChainSyncClient (ClockSkew (..))
import Ouroboros.Consensus.Node
import Ouroboros.Consensus.Node.Tracers (nullTracers)
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
withShelley
  :: ( ByronGiven )
  => ResourceRegistry IO
  -> ChainDB IO (Block ByronConfig)
  -> NodeConfig (BlockProtocol (Block ByronConfig))
  -> NodeState (BlockProtocol (Block ByronConfig))
  -> BlockchainTime IO
  -> (NodeKernel IO Peer (Block ByronConfig) -> ConnectionTable IO -> InitiatorVersions -> ResponderVersions -> IO t)
  -> IO t
withShelley rr cdb conf state blockchainTime k = do
  ctable <- newConnectionTable
  let nodeParams = mkParams rr cdb conf state blockchainTime
  kernel <- nodeKernel nodeParams
  let apps = consensusNetworkApps
        kernel
        nullProtocolTracers
        (protocolCodecs conf)
        (protocolHandlers nodeParams kernel)
      vs = versions apps
  k kernel ctable (initiatorNetworkApplication <$> vs) (responderNetworkApplication <$> vs)

-- | Found in cardano-node that the network magic should be 0.
vData :: NodeToNodeVersionData
vData = NodeToNodeVersionData { networkMagic = 0 }

versions
  :: NetworkApplication IO Peer Lazy.ByteString Lazy.ByteString Lazy.ByteString Lazy.ByteString Lazy.ByteString ()
  -> Versions NodeToNodeVersion DictVersion (NetworkApplication IO Peer Lazy.ByteString Lazy.ByteString Lazy.ByteString Lazy.ByteString Lazy.ByteString ())
versions = simpleSingletonVersions NodeToNodeV_1 vData (DictVersion nodeToNodeCodecCBORTerm)

mkParams
  :: ( ByronGiven )
  => ResourceRegistry IO
  -> ChainDB IO (Block ByronConfig)
  -> NodeConfig (BlockProtocol (Block ByronConfig))
  -> NodeState (BlockProtocol (Block ByronConfig))
  -> BlockchainTime IO
  -> NodeParams IO Peer (Block ByronConfig)
mkParams rr cdb nconf nstate blockchainTime = NodeParams
  { tracers = nullTracers
  , registry = rr
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
