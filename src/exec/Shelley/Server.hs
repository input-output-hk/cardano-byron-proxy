module Shelley.Server
  ( Options (..)
  , runServer
  ) where

import Codec.SerialiseTerm (encodeTerm, decodeTerm)
import Control.Concurrent.Async (wait)
import qualified Network.Socket as Network

import qualified Cardano.Chain.Slotting as Cardano (EpochSlots)
import Cardano.Crypto (ProtocolMagicId)

import Ouroboros.Byron.Proxy.Block (Block)
import Ouroboros.Byron.Proxy.Network.Protocol (responderVersions)
import Ouroboros.Byron.Proxy.ChainSync.Server (chainSyncServer)
import Ouroboros.Network.Protocol.Handshake.Type (Accept (..))
import Ouroboros.Network.Socket (AnyResponderApp (..), newConnectionTable, withServerNode)
import Ouroboros.Storage.ChainDB.API (ChainDB, newHeaderReader)

import Orphans ()

data Options = Options
  { hostName    :: !Network.HostName
  , serviceName :: !Network.ServiceName
  }

-- | Run a Shelley server. It's just chain sync on cardano-ledger blocks.
runServer
  :: Options
  -> ProtocolMagicId
  -> Cardano.EpochSlots
  -> ChainDB IO (Block cfg)
  -> IO ()
runServer serverOptions pm epochSlots db = do
  addrInfos <- Network.getAddrInfo (Just addrInfoHints) (Just host) (Just port)
  tbl <- newConnectionTable
  case addrInfos of
    [] -> error "no getAddrInfo"
    (addrInfo : _) -> do
      reader <- newHeaderReader db
      withServerNode
        tbl
        addrInfo
        encodeTerm
        decodeTerm
        -- TODO: this should be some proper type rather than a tuple
        (,)
        (\_ _ _ -> Accept)
        (fmap AnyResponderApp (responderVersions pm epochSlots (app reader)))
        (\_ -> wait)

  where

  app = \reader -> chainSyncServer db reader

  host = hostName serverOptions
  port = serviceName serverOptions
  addrInfoHints = Network.defaultHints
