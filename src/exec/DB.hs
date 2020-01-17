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
import           Ouroboros.Consensus.Node                  (withChainDB)
import           Ouroboros.Consensus.Protocol              (NodeConfig,
                                                            pbftExtConfig)
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import qualified Ouroboros.Consensus.Util.ResourceRegistry as ResourceRegistry
import           Ouroboros.Storage.ChainDB.API             (ChainDB)
import qualified Ouroboros.Storage.ChainDB.Impl            as ChainDB
import           Ouroboros.Storage.ChainDB.Impl.Args       (ChainDbArgs (..))
import           Ouroboros.Storage.LedgerDB.DiskPolicy     (DiskPolicy (..))

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

  withChainDB dbTracer rr btime (dbFilePath dbOptions) nodeConfig extLedgerState customiseArgs
    $ \cdb ->
      Sqlite.withIndexAuto epochSlots indexTracer (indexFilePath dbOptions) $ \idx -> do
        _ <- ResourceRegistry.forkLinkedThread rr $ Index.trackChainDB rr idx cdb
        k idx cdb

  where

  epochSlots :: Cardano.EpochSlots
  epochSlots = pbftEpochSlots $ pbftExtConfig nodeConfig

  customiseArgs :: ChainDbArgs IO ByronBlock -> ChainDbArgs IO ByronBlock
  customiseArgs args = args
    { -- Don't use the default 'diskPolicy', because that is meant for core
      -- nodes which update the ledger at most once (with one block) per slot
      -- duration while we're updating the ledger with as many blocks as we
      -- can download per slot duration.
      --
      -- When using the default 'diskPolicy', only one snapshot is made every
      -- 12 hours, which is clearly not often enough for the
      -- cardano-byron-proxy. Remember that without a recent snapshot, all the
      -- transactions from the blocks newer than the most-recent on-disk
      -- snapshot have to be replayed against it which requires reading those
      -- blocks and executing the ledger rules, which significantly slows down
      -- startup.
      cdbDiskPolicy = DiskPolicy
        { onDiskNumSnapshots  = 2
          -- Take a snapshot every 20s
        , onDiskWriteInterval = return $ secondsToDiffTime 20
        }
    , cdbBlocksPerFile = 21600 -- ?
    , cdbGcDelay = secondsToDiffTime 20
    }
