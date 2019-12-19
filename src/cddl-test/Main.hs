-- | A test-function that reads EBB-blocks from a chain DB
-- and validates the blocks against a CDDL specification of the CBOR encoding.
-- (CDDL == Concise Data Definition Language)
-- The test can be run as 'cddl-test CDDLCMD SPECFILE DBPATH'.
-- CDDLCMD is the path to CDDL cmd.
-- https://github.com/input-output-hk/cardano-sl/blob/develop/docs/on-the-wire/current-spec.cddl
-- contains a suitable SPECFILE.
module Main
where

import           Prelude hiding ((<>))
import           System.Environment (getArgs)
import           System.Exit (ExitCode(..))
import           System.FilePath ((</>))
import           System.Process.ByteString.Lazy (readProcessWithExitCode)

import           Control.Exception (throwIO, try)
import           Data.Word (Word64)

import           Data.ByteString.Lazy (ByteString)
import           Data.ByteString.Lazy.Char8 as Char8 (unpack)

import           Control.Tracer (nullTracer)
import qualified Cardano.Crypto as Cardano (ProtocolMagicId)
import qualified Cardano.Chain.Slotting as Cardano (EpochSlots (..))
import qualified Ouroboros.Consensus.Ledger.Byron as Byron
import           Ouroboros.Byron.Proxy.Block (ByronBlock, isEBB)

import           Ouroboros.Storage.Common (EpochSize (..),EpochNo(..))
import qualified Ouroboros.Storage.ChainDB.API as ChainDB
import           Ouroboros.Storage.ChainDB.Impl.ImmDB (ImmDB, ImmDbArgs(..), openDB, getBlob)
import           Ouroboros.Storage.FS.API.Types (MountPoint (..))
import           Ouroboros.Storage.FS.IO (ioHasFS)
import           Ouroboros.Storage.EpochInfo.Impl (newEpochInfo)
import           Ouroboros.Storage.ImmutableDB.Types (ImmutableDBError(..), ValidationPolicy(..), UserError (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH (exceptions)

-- hard coded values
epochSize :: Word64
epochSize = 21600
epochSlots :: Cardano.EpochSlots
epochSlots = Cardano.EpochSlots epochSize

givenProtocolMagic :: Cardano.ProtocolMagicId
givenProtocolMagic = error "protocolMagic"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [cddlCmd, spec, dbPath] -> testBlocks cddlCmd spec dbPath Nothing
        [cddlCmd, spec, dbPath, start, end] -> testBlocks cddlCmd spec dbPath $ Just [startEpoch .. endEpoch]
            where
              startEpoch = EpochNo $ read start
              endEpoch   = EpochNo $ read end
        _ -> putStrLn  $ concat [
                "run the tests with :\n"
               ,"   cddl-test CDDLCMD SPECFILE DBPATH\n"
               ,"or cddl-test CDDLCMD SPECFILE DBPATH STARTEPOCH ENDEPOCH\n"
               ]

testBlocks :: FilePath -> FilePath -> FilePath -> Maybe [EpochNo] -> IO ()
testBlocks cddlCmd cddlSpec chainDBPath range = do
    db <- dbArgs chainDBPath >>= openDB
    loop db blockList
    where
        (isLimitedRange, blockList) = case range of
            Nothing -> (False , [EpochNo 0 ..])
            Just r  -> (True  , r)

        loop _ [] = return ()
        loop db (epoch:rest) = do
            putStr $ show  epoch
            block <- readBlock db epoch
            case block of
                NoData -> error $ "failed to read ebb block: " ++ show epoch
                FutureEpoch -> if isLimitedRange
                    then error $ "ebb block is in the future: " ++ show epoch
                    else putStrLn "is the future."
                Block bytes -> do
                    validateCBOR cddlCmd cddlSpec bytes
                    putStrLn " OK"
                    loop db rest

validateCBOR :: FilePath -> FilePath -> ByteString -> IO ()
validateCBOR cddlCmd cddlSpec bytes = do
    result <- readProcessWithExitCode cddlCmd [cddlSpec, "validate", "-"] bytes
    case result of
        (ExitFailure _, _, err) -> error $ Char8.unpack err
        (ExitSuccess, _, _) -> return ()

dbArgs :: FilePath -> IO (ImmDbArgs IO ByronBlock)
dbArgs fp = do
  epochInfo <- newEpochInfo (const (pure $ EpochSize epochSize))
  return $ ImmDbArgs {
      immDecodeHash     = Byron.decodeByronHeaderHash
    , immDecodeBlock    = Byron.decodeByronBlock epochSlots
    , immDecodeHeader   = Byron.decodeByronHeader epochSlots
    , immEncodeHash     = Byron.encodeByronHeaderHash
    , immEncodeBlock    = Byron.encodeByronBlockWithInfo
    , immHashInfo       = Byron.byronHashInfo
    , immErr            = EH.exceptions
    , immEpochInfo      = epochInfo
    , immValidation     = ValidateMostRecentEpoch
    , immIsEBB          = isEBB
    , immAddHdrEnv      = Byron.byronAddHeaderEnvelope
    , immCheckIntegrity = const True -- No validation
    , immHasFS          = ioHasFS $ MountPoint (fp </> "immutable")
    , immTracer         = nullTracer
    }

data ReadResult = Block ByteString | FutureEpoch | NoData
  deriving (Show, Eq)

readBlock :: ImmDB IO ByronBlock -> EpochNo -> IO ReadResult
readBlock db epoch = do
    ret <- try $ getBlob db ChainDB.Block $ Left epoch
    case ret of
        (Left (UserError (ReadFutureEBBError _ _) _)) -> return FutureEpoch
        (Left err) -> throwIO err
        (Right Nothing) -> return NoData
        (Right (Just (_hash, block))) -> return $ Block block
