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
import           Ouroboros.Consensus.Block                 (GetHeader (Header))
import           Ouroboros.Consensus.BlockchainTime        (BlockchainTime)
import           Ouroboros.Consensus.Ledger.Extended       (ExtLedgerState)
import           Ouroboros.Consensus.Node                  (openChainDB)
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import qualified Ouroboros.Consensus.Util.ResourceRegistry as ResourceRegistry
import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Args (ChainDbArgs (..))
import           Ouroboros.Consensus.Config                (TopLevelConfig, configBlock)
import           Ouroboros.Consensus.Byron.Ledger.Config   (byronEpochSlots)
import           Ouroboros.Consensus.Byron.Node ()
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Types as ChainDB (TraceEvent)
import           Ouroboros.Consensus.Storage.VolatileDB.Types (mkBlocksPerFile)

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
  -> TopLevelConfig ByronBlock
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
  epochSlots = byronEpochSlots $ configBlock nodeConfig

  customiseArgs :: ChainDbArgs IO ByronBlock -> ChainDbArgs IO ByronBlock
  customiseArgs args = args
    { cdbBlocksPerFile = mkBlocksPerFile 21600 -- ?
    , cdbGcDelay = secondsToDiffTime 20
    }
