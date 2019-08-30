{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTSyntax          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB
  ( DBConfig (..)
  , withDB
  ) where

import           Control.Exception                         (bracket)
import           Control.Tracer                            (Tracer)
import qualified Data.Reflection                           as Reflection (given)
import           Data.Time.Clock                           (secondsToDiffTime)
import qualified System.Directory                          (createDirectoryIfMissing)
import           System.FilePath                           ((</>))

import qualified Cardano.Chain.Slotting                    as Cardano (EpochSlots (..))

import           Ouroboros.Byron.Proxy.Block               (Block, isEBB)
import qualified Ouroboros.Byron.Proxy.Index.ChainDB       as Index (trackChainDB)
import qualified Ouroboros.Byron.Proxy.Index.Sqlite        as Sqlite
import           Ouroboros.Byron.Proxy.Index.Types         (Index)
import           Ouroboros.Consensus.Block                 (BlockProtocol,
                                                            GetHeader (Header))
import           Ouroboros.Consensus.Ledger.Byron          (ByronGiven)
import qualified Ouroboros.Consensus.Ledger.Byron          as Byron
import           Ouroboros.Consensus.Ledger.Byron.Config   (ByronConfig)
import           Ouroboros.Consensus.Ledger.Extended       (ExtLedgerState)
import           Ouroboros.Consensus.Protocol              (NodeConfig)
import           Ouroboros.Consensus.Protocol.Abstract     (SecurityParam (..))
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import qualified Ouroboros.Consensus.Util.ResourceRegistry as ResourceRegistry
import           Ouroboros.Storage.ChainDB.API             (ChainDB)
import qualified Ouroboros.Storage.ChainDB.API             as ChainDB
import qualified Ouroboros.Storage.ChainDB.Impl            as ChainDB
import           Ouroboros.Storage.ChainDB.Impl.Args       (ChainDbArgs (..))
import           Ouroboros.Storage.Common                  (EpochSize (..))
import           Ouroboros.Storage.EpochInfo.Impl          (newEpochInfo)
import           Ouroboros.Storage.FS.API.Types            (MountPoint (..))
import           Ouroboros.Storage.FS.IO                   (ioHasFS)
import           Ouroboros.Storage.ImmutableDB.Types       (ValidationPolicy (..))
import           Ouroboros.Storage.LedgerDB.DiskPolicy     (DiskPolicy (..))
import           Ouroboros.Storage.LedgerDB.MemPolicy      (defaultMemPolicy)
import qualified Ouroboros.Storage.Util.ErrorHandling      as EH

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
     ( ByronGiven ) -- For HasHeader instances
  => DBConfig
  -> Tracer IO (ChainDB.TraceEvent (Block ByronConfig))
  -> Tracer IO Sqlite.TraceEvent
  -> ResourceRegistry IO
  -> SecurityParam
  -> NodeConfig (BlockProtocol (Block ByronConfig))
  -> ExtLedgerState (Block ByronConfig)
  -> (Index IO (Header (Block ByronConfig)) -> ChainDB IO (Block ByronConfig) -> IO t)
  -> IO t
withDB dbOptions dbTracer indexTracer rr securityParam nodeConfig extLedgerState k = do
  -- The ChainDB/Storage layer will not create a directory for us, we have
  -- to ensure it exists.
  System.Directory.createDirectoryIfMissing True (dbFilePath dbOptions)
  let epochSlots :: Cardano.EpochSlots
      epochSlots = Reflection.given
      epochSize = EpochSize $
        fromIntegral (Cardano.unEpochSlots epochSlots)
  epochInfo <- newEpochInfo (const (pure epochSize))
  let fp = dbFilePath dbOptions
      -- Don't use the default 'diskPolicy', because that is meant for core
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
      ledgerDiskPolicy = DiskPolicy
        { onDiskNumSnapshots  = 2
          -- Take a snapshot every 20s
        , onDiskWriteInterval = return $ secondsToDiffTime 20
        }
      chainDBArgs :: ChainDbArgs IO (Block ByronConfig)
      chainDBArgs = ChainDB.ChainDbArgs
        { cdbDecodeHash = Byron.decodeByronHeaderHash
        , cdbEncodeHash = Byron.encodeByronHeaderHash

        , cdbDecodeBlock = Byron.decodeByronBlock epochSlots
        , cdbEncodeBlock = Byron.encodeByronBlock

        , cdbDecodeLedger = Byron.decodeByronLedgerState
        , cdbEncodeLedger = Byron.encodeByronLedgerState

        , cdbDecodeChainState = Byron.decodeByronChainState
        , cdbEncodeChainState = Byron.encodeByronChainState

        , cdbErrImmDb = EH.exceptions
        , cdbErrVolDb = EH.exceptions
        , cdbErrVolDbSTM = EH.throwSTM

        , cdbHasFSImmDb = ioHasFS $ MountPoint (fp </> "immutable")
        , cdbHasFSVolDb = ioHasFS $ MountPoint (fp </> "volatile")
        , cdbHasFSLgrDB = ioHasFS $ MountPoint (fp </> "ledger")

        , cdbValidation = ValidateMostRecentEpoch
        , cdbBlocksPerFile = 21600 -- ?
        , cdbMemPolicy = defaultMemPolicy securityParam
        , cdbDiskPolicy = ledgerDiskPolicy

        , cdbNodeConfig = nodeConfig
        , cdbEpochInfo = epochInfo
        , cdbIsEBB = isEBB
        , cdbGenesis = pure extLedgerState

        , cdbTracer = dbTracer
        , cdbRegistry = rr
        , cdbGcDelay = secondsToDiffTime 20
        }
  bracket (ChainDB.openDB chainDBArgs) ChainDB.closeDB $ \cdb ->
    Sqlite.withIndexAuto epochSlots indexTracer (indexFilePath dbOptions) $ \idx -> do
      -- TBD do we need withRegistry in there or not?
      -- _ <- ResourceRegistry.forkLinkedThread rr $ ResourceRegistry.withRegistry $ \rr' -> Index.trackChainDB rr' idx cdb
      _ <- ResourceRegistry.forkLinkedThread rr $ Index.trackChainDB rr idx cdb
      k idx cdb
