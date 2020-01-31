{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTSyntax          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB
  ( DBConfig (..)
  , withDB
  ) where

import           Control.Tracer                            (Tracer)
import           Data.Time.Clock                           (secondsToDiffTime)
import qualified System.Directory                          (createDirectoryIfMissing)

import qualified Cardano.Chain.Slotting                    as Cardano (EpochSlots (..))

import           Ouroboros.Byron.Proxy.Block               (ByronBlock)
import qualified Ouroboros.Byron.Proxy.Index.ChainDB       as Index (trackChainDB)
import qualified Ouroboros.Byron.Proxy.Index.Sqlite        as Sqlite
import           Ouroboros.Byron.Proxy.Index.Types         (Index)
import           Ouroboros.Consensus.Block                 (BlockProtocol,
                                                            GetHeader (Header))
import           Ouroboros.Consensus.BlockchainTime        (BlockchainTime)
import           Ouroboros.Consensus.Ledger.Byron.Config   (pbftEpochSlots)
import           Ouroboros.Consensus.Ledger.Extended       (ExtLedgerState)
import           Ouroboros.Consensus.Node                  (openChainDB)
import           Ouroboros.Consensus.Protocol              (NodeConfig)
import           Ouroboros.Consensus.Protocol.ExtConfig    (extNodeConfig)
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import qualified Ouroboros.Consensus.Util.ResourceRegistry as ResourceRegistry
import           Ouroboros.Storage.ChainDB                 (ChainDB)
import qualified Ouroboros.Storage.ChainDB                 as ChainDB
import           Ouroboros.Storage.ChainDB.Impl.Args       (ChainDbArgs (..))

data DBConfig = DBConfig
  { dbFilePath    :: !FilePath
    -- ^ Directory to house the `ImmutableDB`.
  , indexFilePath :: !FilePath
    -- ^ Path to the SQLite index.
  }

-- | Set up and use a DB.
--
-- The directory at `dbFilePath` will be created if it does not exist.
withDB
  :: forall t .
     DBConfig
  -> Tracer IO (ChainDB.TraceEvent ByronBlock)
  -> Tracer IO Sqlite.TraceEvent
  -> ResourceRegistry IO
  -> BlockchainTime IO
  -> NodeConfig (BlockProtocol ByronBlock)
  -> ExtLedgerState ByronBlock
  -> (Index IO (Header ByronBlock) -> ChainDB IO ByronBlock -> IO t)
  -> IO t
withDB dbOptions dbTracer indexTracer rr btime nodeConfig extLedgerState k = do
  -- The ChainDB/Storage layer will not create a directory for us, we have
  -- to ensure it exists.
  System.Directory.createDirectoryIfMissing True (dbFilePath dbOptions)

  (_, cdb) <- ResourceRegistry.allocate rr
    (\_ -> openChainDB dbTracer rr btime (dbFilePath dbOptions) nodeConfig extLedgerState customiseArgs)
    ChainDB.closeDB
  Sqlite.withIndexAuto epochSlots indexTracer (indexFilePath dbOptions) $ \idx -> do
    _ <- ResourceRegistry.forkLinkedThread rr $ Index.trackChainDB rr idx cdb
    k idx cdb

  where

  epochSlots :: Cardano.EpochSlots
  epochSlots = pbftEpochSlots $ extNodeConfig nodeConfig

  customiseArgs :: ChainDbArgs IO ByronBlock -> ChainDbArgs IO ByronBlock
  customiseArgs args = args
    { cdbBlocksPerFile = 21600 -- ?
    , cdbGcDelay = secondsToDiffTime 20
    }
