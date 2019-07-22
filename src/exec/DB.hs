{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module DB
  ( DBConfig (..)
  , withDB
  ) where

import Control.Exception (bracket)
import qualified Data.Reflection as Reflection (given)
import Control.Tracer (Tracer)
import Data.Time.Clock (secondsToDiffTime)
import qualified System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import qualified Cardano.Chain.Genesis as Cardano.Genesis (Config)
import qualified Cardano.Chain.Slotting as Cardano (EpochSlots (..))
import qualified Cardano.Crypto as Crypto (ProtocolMagicId)

import           Ouroboros.Byron.Proxy.Block (Block, isEBB)
import qualified Ouroboros.Byron.Proxy.Block as Byron.Proxy
import           Ouroboros.Byron.Proxy.Index.Types (Index)
import qualified Ouroboros.Byron.Proxy.Index.ChainDB as Index (trackChainDB)
import qualified Ouroboros.Byron.Proxy.Index.Sqlite as Sqlite
import Ouroboros.Consensus.Block (GetHeader (Header))
import Ouroboros.Consensus.Ledger.Byron (ByronGiven)
import Ouroboros.Consensus.Ledger.Byron.Config (ByronConfig)
import qualified Ouroboros.Consensus.Ledger.Byron as Byron
import Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import qualified Ouroboros.Consensus.Protocol.PBFT as PBFT
import Ouroboros.Consensus.Node.ProtocolInfo.Abstract (ProtocolInfo (..), NumCoreNodes (..))
import Ouroboros.Consensus.Node.ProtocolInfo.Byron (protocolInfoByron)
import Ouroboros.Consensus.Util.ThreadRegistry (ThreadRegistry, withThreadRegistry)
import Ouroboros.Storage.FS.API.Types (MountPoint (..))
import Ouroboros.Storage.FS.IO (ioHasFS)
import Ouroboros.Storage.Common (EpochSize (..))
import Ouroboros.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Storage.ChainDB.API as ChainDB
import qualified Ouroboros.Storage.ChainDB.Impl as ChainDB
import Ouroboros.Storage.ChainDB.Impl.Args (ChainDbArgs (..))
import Ouroboros.Storage.ImmutableDB.Types (ValidationPolicy (..))
import Ouroboros.Storage.LedgerDB.DiskPolicy (defaultDiskPolicy)
import Ouroboros.Storage.LedgerDB.MemPolicy (defaultMemPolicy)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

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
  :: forall cfg t .
     ( ByronGiven ) -- For HasHeader instances
  => Cardano.Genesis.Config
  -> DBConfig
  -> Tracer IO (ChainDB.TraceEvent (Block ByronConfig))
  -> (Index IO (Header (Block ByronConfig)) -> ChainDB IO (Block ByronConfig) -> IO t)
  -> IO t
withDB genesisConfig dbOptions tracer k = do
  -- The ChainDB/Storage layer will not create a directory for us, we have
  -- to ensure it exists.
  System.Directory.createDirectoryIfMissing True (dbFilePath dbOptions)
  let epochSlots :: Cardano.EpochSlots
      epochSlots = Reflection.given
      pm :: Crypto.ProtocolMagicId
      pm = Reflection.given
      epochSize = EpochSize $
        fromIntegral (Cardano.unEpochSlots epochSlots)
      fp = dbFilePath dbOptions
      securityParam = SecurityParam 2160
      numCoreNodes = NumCoreNodes 42
      coreNodeId = undefined -- Doesn't seem to matter
      pbftParams = PBFT.PBftParams
        { PBFT.pbftSecurityParam = securityParam
        , PBFT.pbftNumNodes = 7
        , PBFT.pbftSignatureWindow = 2160
        , PBFT.pbftSignatureThreshold = 0.22
        }
      protocolInfo = protocolInfoByron numCoreNodes coreNodeId pbftParams genesisConfig Nothing
      chainDBArgs :: ThreadRegistry IO -> ChainDbArgs IO (Block ByronConfig)
      chainDBArgs = \threadRegistry -> ChainDB.ChainDbArgs
        { cdbDecodeHash = Byron.decodeByronHeaderHash
        , cdbEncodeHash = Byron.encodeByronHeaderHash

        -- Must use a CBOR-in-CBOR codec for blocks, so that we don't lose the
        -- EBB body data, which Byron peers require. The codec from
        -- Ouroboros.Consensus.Ledger.Byron always encodes with empty body
        -- and attributes, so it does not necessarily invert the decoder.
        , cdbDecodeBlock = Byron.Proxy.decodeBlock epochSlots
        , cdbEncodeBlock = Byron.Proxy.encodeBlock

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
        , cdbDiskPolicy = defaultDiskPolicy securityParam (secondsToDiffTime 20)

        , cdbNodeConfig = pInfoConfig protocolInfo
        , cdbEpochSize = const (pure epochSize)
        , cdbIsEBB = isEBB
        , cdbGenesis = pure (pInfoInitLedger protocolInfo)

        , cdbTracer = tracer
        , cdbThreadRegistry = threadRegistry
        , cdbGcDelay = secondsToDiffTime 20
        }
  withThreadRegistry $ \tr ->
    bracket (ChainDB.openDB (chainDBArgs tr)) ChainDB.closeDB $ \cdb ->
      Sqlite.withIndexAuto epochSlots (indexFilePath dbOptions) $ \idx ->
        -- TODO replace seq with $ to run with the index. It will fail because
        -- of a suspected ChainDB bug.
        Index.trackChainDB idx cdb $ k idx cdb
