{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Shelley where

import qualified Data.ByteString.Lazy as Lazy
import Data.Void (Void)

-- ToCBOR/FromCBOR UTxOValidationError, for local tx submission codec.
import Cardano.Chain.UTxO.Validation ()
import qualified Network.Socket as Socket

import Cardano.Prelude (Proxy(..))

import Ouroboros.Byron.Proxy.Block (ByronBlock)
import Ouroboros.Consensus.Block (BlockProtocol)
import Ouroboros.Consensus.Protocol (NodeConfig, NodeState)
import Ouroboros.Consensus.Mempool.Impl (MempoolCapacityBytes (..))
import Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import Ouroboros.Storage.ChainDB.API (ChainDB)

import Ouroboros.Network.NodeToNode
import Ouroboros.Network.Mux
import Ouroboros.Network.Protocol.ChainSync.PipelineDecision (pipelineDecisionLowHighMark)
import Ouroboros.Network.Protocol.Handshake.Version
import Ouroboros.Network.Server.ConnectionTable (ConnectionTable, newConnectionTable)
import Ouroboros.Consensus.BlockchainTime (BlockchainTime)
import Ouroboros.Consensus.ChainSyncClient (ClockSkew (..))
import Ouroboros.Consensus.Node
import Ouroboros.Consensus.NodeKernel
import Ouroboros.Consensus.Node.Tracers (nullTracers)
import Ouroboros.Consensus.Node.Run.Abstract (nodeBlockFetchSize, nodeBlockMatchesHeader)
import Ouroboros.Consensus.NodeNetwork

type InitiatorVersions = Versions NodeToNodeVersion DictVersion (OuroborosApplication 'InitiatorApp ConnectionId NodeToNodeProtocols IO Lazy.ByteString () Void)

type ResponderVersions = Versions NodeToNodeVersion DictVersion (OuroborosApplication 'ResponderApp ConnectionId NodeToNodeProtocols IO Lazy.ByteString Void ())

-- Must have `ByronGiven` because of the constraints on `nodeKernel`
withShelley
  :: ResourceRegistry IO
  -> ChainDB IO ByronBlock
  -> NodeConfig (BlockProtocol ByronBlock)
  -> NodeState (BlockProtocol ByronBlock)
  -> BlockchainTime IO
  -> (NodeKernel IO ConnectionId ByronBlock -> ConnectionTable IO Socket.SockAddr -> InitiatorVersions -> ResponderVersions -> IO t)
  -> IO t
withShelley rr cdb conf state blockchainTime k = do
  ctable <- newConnectionTable
  let nodeParams = mkParams rr cdb conf state blockchainTime
  kernel <- initNodeKernel nodeParams
  let apps = consensusNetworkApps
        kernel
        nullProtocolTracers
        (protocolCodecs conf)
        (protocolHandlers nodeParams kernel)
      vs = versions conf apps
  k kernel ctable (initiatorNetworkApplication <$> vs) (responderNetworkApplication <$> vs)

versions
  :: NodeConfig (BlockProtocol ByronBlock) -> NetworkApplication IO ConnectionId Lazy.ByteString Lazy.ByteString Lazy.ByteString Lazy.ByteString Lazy.ByteString ()
  -> Versions NodeToNodeVersion DictVersion (NetworkApplication IO ConnectionId Lazy.ByteString Lazy.ByteString Lazy.ByteString Lazy.ByteString Lazy.ByteString ())
versions conf = simpleSingletonVersions NodeToNodeV_1 (NodeToNodeVersionData (nodeNetworkMagic (Proxy @ByronBlock) conf)) (DictVersion nodeToNodeCodecCBORTerm)

mkParams
  :: ResourceRegistry IO
  -> ChainDB IO ByronBlock
  -> NodeConfig (BlockProtocol ByronBlock)
  -> NodeState (BlockProtocol ByronBlock)
  -> BlockchainTime IO
  -> NodeArgs IO ConnectionId ByronBlock
mkParams rr cdb nconf nstate blockchainTime = NodeArgs
  { tracers = nullTracers
  , registry = rr
  , maxClockSkew = ClockSkew 1
  , cfg = nconf
  , initState = nstate
  , btime = blockchainTime
  , chainDB = cdb
  , blockProduction = Nothing
  , blockFetchSize = nodeBlockFetchSize
  , blockMatchesHeader = nodeBlockMatchesHeader
  , maxUnackTxs = maxBound
  , maxBlockBodySize = maxBound
  , chainSyncPipelining = pipelineDecisionLowHighMark 200 300 -- TODO: make configurable!
  , mempoolCap = MempoolCapacityBytes 128_000
  }
