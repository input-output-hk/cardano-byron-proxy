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
import Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import Ouroboros.Consensus.Config (TopLevelConfig)
import Ouroboros.Network.NodeToNode
import Ouroboros.Network.Mux
import Ouroboros.Network.Protocol.Handshake.Version
import Ouroboros.Network.Server.ConnectionTable (ConnectionTable, newConnectionTable)
import Ouroboros.Consensus.BlockchainTime (BlockchainTime)
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client (ClockSkew (..))
import Ouroboros.Consensus.Node
import Ouroboros.Consensus.Node.State (NodeState)
import Ouroboros.Consensus.NodeKernel
import Ouroboros.Consensus.Node.Tracers (nullTracers)
import Ouroboros.Consensus.Node.Run (nodeBlockFetchSize, nodeBlockMatchesHeader)
import Ouroboros.Consensus.NodeNetwork
import Ouroboros.Consensus.Byron.Node ()  -- Run instance
import Ouroboros.Consensus.Byron.Ledger.NetworkProtocolVersion
                                   (ByronNetworkProtocolVersion(ByronNetworkProtocolVersion1))

type InitiatorVersions = Versions
                              NodeToNodeVersion
                              DictVersion
                              (ConnectionId Socket.SockAddr
                               -> OuroborosApplication 'InitiatorApp Lazy.ByteString IO () Void)
type ResponderVersions = Versions
                              NodeToNodeVersion
                              DictVersion
                              (ConnectionId Socket.SockAddr
                               -> OuroborosApplication 'ResponderApp Lazy.ByteString IO Void ())

-- Must have `ByronGiven` because of the constraints on `nodeKernel`

withShelley
  :: ResourceRegistry IO
  -> ChainDB IO ByronBlock
  -> TopLevelConfig ByronBlock
  -> NodeState ByronBlock
  -> BlockchainTime IO
  -> (NodeKernel IO (ConnectionId Socket.SockAddr) ByronBlock
      -> ConnectionTable IO Socket.SockAddr
      -> InitiatorVersions
      -> ResponderVersions -> IO t)
  -> IO t
withShelley rr cdb conf state blockchainTime k = do
  ctable <- newConnectionTable
  let nodeParams = mkParams rr cdb conf state blockchainTime
      miniconf = miniProtocolParameters nodeParams
  kernel <- initNodeKernel nodeParams
  let apps = consensusNetworkApps
        kernel
        nullProtocolTracers
        (protocolCodecs conf ByronNetworkProtocolVersion1)
        (protocolHandlers nodeParams kernel)
      vs = versions conf apps
  k kernel ctable
     (initiatorNetworkApplication miniconf <$> vs)
     (responderNetworkApplication miniconf <$> vs)

versions
  :: TopLevelConfig ByronBlock
  -> r
  -> Versions NodeToNodeVersion DictVersion r

versions conf = simpleSingletonVersions NodeToNodeV_1 (NodeToNodeVersionData (nodeNetworkMagic (Proxy @ByronBlock) conf)) (DictVersion nodeToNodeCodecCBORTerm)

mkParams
  :: ResourceRegistry IO
  -> ChainDB IO ByronBlock
  -> TopLevelConfig ByronBlock
  -> NodeState ByronBlock
  -> BlockchainTime IO
  -> NodeArgs IO (ConnectionId Socket.SockAddr) ByronBlock
mkParams rr cdb nconf nstate blockchainTime = NodeArgs
  { tracers = nullTracers
  , registry = rr
  , maxClockSkew = ClockSkew 1
  , cfg = nconf
  , initState = nstate
  , btime = blockchainTime
  , chainDB = cdb
  , blockProduction = Nothing
  -- Don't automatically add a dummy genesis EBB, we'll add the real one
  -- later on
  , initChainDB = \_cfg _cdb -> return ()
  , blockFetchSize = nodeBlockFetchSize
  , blockMatchesHeader = nodeBlockMatchesHeader
  , maxBlockSize = MaxBlockBodySize maxBound
  , mempoolCap = NoMempoolCapacityBytesOverride
  , miniProtocolParameters = defaultMiniProtocolParameters
  }
