module Cardano.Client.Byron
(
   module Cardano.Client.Byron
  ,module Ouroboros.Consensus.Storage.ChainDB.API
  ,module Ouroboros.Consensus.Storage.ChainDB.Impl.Args
  ,module Ouroboros.Consensus.Block
  ,module Ouroboros.Consensus.Config
  ,module Ouroboros.Consensus.Byron.Ledger.Config
  ,module Ouroboros.Consensus.Byron.Ledger.NetworkProtocolVersion
  ,module Ouroboros.Network.Block
  ,module Ouroboros.Network.Point
  ,module Ouroboros.Consensus.Util.ResourceRegistry
  )
where
import           Ouroboros.Consensus.Storage.ChainDB.API                 as ChainDB (ChainDB, Reader)
import           Ouroboros.Consensus.Storage.ChainDB.API hiding (ChainDB)
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Args           as ChainDB (ChainDbArgs (..))
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Args hiding (ChainDbArgs)

import qualified Ouroboros.Consensus.Block                               as OC
import           Ouroboros.Consensus.Block hiding (Header)
import           Ouroboros.Consensus.Byron.Ledger.Block                  (ByronBlock)

import qualified Ouroboros.Network.Block as ON (HeaderHash, ChainHash (..), Point)
import           Ouroboros.Network.Block hiding (ChainHash, HeaderHash, Point, getTipPoint, getPoint, getTipBlockNo)

import           Ouroboros.Network.Point
import qualified Ouroboros.Consensus.Config                              as Node (TopLevelConfig)
import           Ouroboros.Consensus.Config hiding (TopLevelConfig)

import           Ouroboros.Consensus.Byron.Ledger.Config
import           Ouroboros.Consensus.Byron.Ledger.NetworkProtocolVersion
import qualified Ouroboros.Consensus.Ledger.Extended                     as Node (ExtLedgerState)
import qualified Ouroboros.Consensus.Mempool.API                         as Node (Mempool)
import qualified Ouroboros.Consensus.Mempool.TxSeq                       as Node (TicketNo)
import qualified Ouroboros.Consensus.NodeKernel                          as Node
import qualified Ouroboros.Consensus.Node.State                          as Node (NodeState)


--import           Ouroboros.Consensus.Byron.Node                          ()

import           Ouroboros.Consensus.Util.ResourceRegistry               (ResourceRegistry)

import qualified Ouroboros.Network.Socket as ON (ConnectionId)
{-
import           Ouroboros.Consensus.Block                               (GetHeader (Header))

import           Ouroboros.Consensus.Storage.Common                      (BlockComponent (..))
import           Ouroboros.Consensus.Util.ResourceRegistry               (ResourceRegistry)
import           Ouroboros.Network.Block                                 (ChainUpdate (..),
                                                                          HeaderHash,
                                                                          Point (..),
                                                                          SlotNo (..))

import           Ouroboros.Network.Point                                 (Block (..),
                                                                          WithOrigin (..),
                                                                          blockPointHash,
                                                                          blockPointSlot)

import           Network.Mux.Trace                                       (MuxError (..),
                                                                          MuxErrorType (..))
import qualified Ouroboros.Consensus.Block                               (GetHeader (..))
import           Ouroboros.Consensus.BlockchainTime                      (SlotLength (..),
                                                                          SystemStart (..),
                                                                          focusSlotLengths,
                                                                          realBlockchainTime,
                                                                          singletonSlotLengths)
import           Ouroboros.Consensus.Byron.Ledger.Serialisation          (encodeByronBlock)
import           Ouroboros.Consensus.Byron.Node                          (PBftSignatureThreshold (..),
                                                                          protocolInfoByron)
import           Ouroboros.Consensus.Config                              (configSecurityParam)
import           Ouroboros.Consensus.Mempool.API                         (Mempool)
import           Ouroboros.Consensus.Mempool.TxSeq                       (TicketNo)
import           Ouroboros.Consensus.Node                                (getMempool)
import           Ouroboros.Consensus.Node.ErrorPolicy                    (consensusErrorPolicy)
import           Ouroboros.Consensus.Node.ProtocolInfo                   (ProtocolInfo (..))
import           Ouroboros.Consensus.Node.Run                            ()
import           Ouroboros.Consensus.Protocol.Abstract                   (SecurityParam (..))
import           Ouroboros.Consensus.Storage.ChainDB.API                 (ChainDB)
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Types          (TraceAddBlockEvent (..),
                                                                          TraceEvent (..),
                                                                          TraceValidationEvent (..))
import           Ouroboros.Consensus.Util.ResourceRegistry               (ResourceRegistry,
                                                                          withRegistry)
import           Ouroboros.Network.Block                                 (BlockNo (..))
import           Ouroboros.Network.BlockFetch.Client                     (BlockFetchProtocolFailure)
import           Ouroboros.Network.ErrorPolicy                           (ErrorPolicyTrace (..),
                                                                          WithAddr (..))
import           Ouroboros.Network.ErrorPolicy                           (SuspendDecision (..))
import           Ouroboros.Network.NodeToNode
import           Ouroboros.Network.Protocol.Handshake.Type               (HandshakeClientProtocolError)
import           Ouroboros.Network.Server.ConnectionTable                (ConnectionTable)
import qualified Ouroboros.Network.TxSubmission.Inbound                  as TxInbound
import qualified Ouroboros.Network.TxSubmission.Outbound                 as TxOutbound

import           Ouroboros.Consensus.Block                               (GetHeader (Header))
import           Ouroboros.Consensus.BlockchainTime                      (BlockchainTime)
import           Ouroboros.Consensus.Ledger.Extended                     (ExtLedgerState)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client       (ClockSkew (..))
import           Ouroboros.Consensus.Node                                (openChainDB)
import           Ouroboros.Consensus.Node.Run                            (nodeBlockFetchSize,
                                                                          nodeBlockMatchesHeader)
import           Ouroboros.Consensus.Node.State                          (NodeState)
import           Ouroboros.Consensus.Node.Tracers                        (nullTracers)
import           Ouroboros.Consensus.NodeKernel
import           Ouroboros.Consensus.NodeNetwork
import           Ouroboros.Consensus.Protocol.Abstract                   (SecurityParam (maxRollbacks))
import           Ouroboros.Consensus.Storage.ChainDB.API                 (ChainDB)
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Args           (ChainDbArgs (..))
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Types          as ChainDB
                                                                                     (TraceEvent)
import           Ouroboros.Consensus.Storage.VolatileDB.Types            (mkBlocksPerFile)
import           Ouroboros.Consensus.Util.ResourceRegistry               (ResourceRegistry)
import qualified Ouroboros.Network.AnchoredFragment                      as AF
import qualified Ouroboros.Network.ChainFragment                         as CF
import           Ouroboros.Network.IOManager                             (AssociateWithIOCP,
                                                                          withIOManager)
import           Ouroboros.Network.Mux
import           Ouroboros.Network.NodeToNode
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Server.ConnectionTable                (ConnectionTable,
                                                                          newConnectionTable)
import           Ouroboros.Network.Snocket                               (socketSnocket)
import           Ouroboros.Network.Socket
-}

type ChainDB        = ChainDB.ChainDB IO     ByronBlock
type ChainDbArgs    = ChainDB.ChainDbArgs IO ByronBlock
type HeaderReader   = ChainDB.Reader IO ByronBlock Header

type TopLevelConfig  = Node.TopLevelConfig ByronBlock
type NodeState       = Node.NodeState ByronBlock
type NodeKernel addr = Node.NodeKernel IO (ON.ConnectionId addr) ByronBlock
type NodeArgs   addr = Node.NodeArgs IO   (ON.ConnectionId addr) ByronBlock
type ExtLedgerState  = Node.ExtLedgerState ByronBlock
type Mempool         = Node.Mempool IO ByronBlock Node.TicketNo

type Header          = OC.Header ByronBlock

type Point          = ON.Point (OC.Header ByronBlock)
type ChainHash      = ON.ChainHash ByronBlock
type HeaderHash     = ON.HeaderHash ByronBlock
