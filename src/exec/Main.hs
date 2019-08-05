{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE PatternSynonyms #-}

{-# LANGUAGE TypeFamilies #-}

import Codec.SerialiseTerm (decodeTerm, encodeTerm)
import Control.Exception (throwIO)
import Control.Monad (mapM, void)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Class.MonadAsync (concurrently, wait)
import Control.Tracer (Tracer (..), contramap, nullTracer, traceWith)
import Data.Functor.Contravariant (Op (..))
import Data.List (intercalate)
import qualified Data.Reflection as Reflection (give)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Text
import qualified Network.Socket as Network
import qualified Options.Applicative as Opt
import System.FilePath (takeDirectory)
import System.IO.Error (userError)

import qualified Cardano.BM.Data.Aggregated as Monitoring
import qualified Cardano.BM.Data.LogItem as Monitoring
import qualified Cardano.BM.Data.Severity as Monitoring

import qualified Cardano.Chain.Genesis as Cardano
import qualified Cardano.Chain.Slotting as Cardano
import qualified Cardano.Chain.Update as Cardano
import qualified Cardano.Crypto as Cardano

import qualified Pos.Chain.Block as CSL (genesisBlock0)
import qualified Pos.Chain.Block as CSL (recoveryHeadersMessage, streamWindow,
                                         withBlockConfiguration)
import qualified Pos.Chain.Delegation as CSL
import qualified Pos.Chain.Lrc as CSL (genesisLeaders)
import qualified Pos.Chain.Genesis as CSL.Genesis (Config)
import qualified Pos.Chain.Genesis as CSL
import qualified Pos.Chain.Update as CSL
import qualified Pos.Chain.Ssc as CSL (withSscConfiguration)
import qualified Pos.Configuration as CSL (networkConnectionTimeout, withNodeConfiguration)
import qualified Pos.Crypto as CSL
import qualified Pos.Diffusion.Full as CSL (FullDiffusionConfiguration (..))

import qualified Pos.Infra.Network.CLI as CSL (NetworkConfigOpts (..),
                                               externalNetworkAddressOption,
                                               intNetworkConfigOpts,
                                               listenNetworkAddressOption)
import Pos.Infra.Network.Types (NetworkConfig (..))
import qualified Pos.Infra.Network.Policy as Policy
import qualified Pos.Launcher.Configuration as CSL (Configuration (..),
                                                    ConfigurationOptions (..),
                                                    HasConfigurations)
import qualified Pos.Client.CLI.Options as CSL (configurationOptionsParser)
import qualified Pos.Util.Config as CSL (parseYamlConfig)

import Pos.Util.Trace (Trace)
import qualified Pos.Util.Trace.Named as Trace (LogNamed (..), appendName, named)
import qualified Pos.Util.Trace
import qualified Pos.Util.Wlog as Wlog

import Ouroboros.Byron.Proxy.Block (Block)
import Ouroboros.Byron.Proxy.Index.Types (Index)
import qualified Ouroboros.Byron.Proxy.Index.Sqlite as Index (TraceEvent)
import Ouroboros.Byron.Proxy.Genesis.Convert
import Ouroboros.Byron.Proxy.Main
import Ouroboros.Consensus.Block (GetHeader (Header))
import Ouroboros.Consensus.BlockchainTime (SlotLength (..), SystemStart (..), realBlockchainTime)
import Ouroboros.Consensus.Ledger.Byron (ByronGiven)
import Ouroboros.Consensus.Ledger.Byron.Config (ByronConfig)
import Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import Ouroboros.Consensus.Node.ProtocolInfo.Abstract (ProtocolInfo (..))
import Ouroboros.Consensus.Node.ProtocolInfo.Byron (protocolInfoByron)
import Ouroboros.Network.NodeToNode (withServer)
import Ouroboros.Consensus.Util.ThreadRegistry (ThreadRegistry, withThreadRegistry)
import Ouroboros.Network.Block (SlotNo (..), Point (..))
import Ouroboros.Network.Point (WithOrigin (..))
import qualified Ouroboros.Network.Point as Point (Block (..))
import Ouroboros.Network.Protocol.Handshake.Type (acceptEq)
import Ouroboros.Network.Protocol.Handshake.Version (DictVersion (..))
import Ouroboros.Network.Server.ConnectionTable (ConnectionTable)
import Ouroboros.Network.Socket (connectToNode')
import Ouroboros.Network.Subscription.Common (IPSubscriptionTarget (..), ipSubscriptionWorker)
import Ouroboros.Storage.ChainDB.API (ChainDB)
import Ouroboros.Storage.ChainDB.Impl.Types (TraceEvent (..), TraceAddBlockEvent (..), TraceValidationEvent (..))

import qualified Byron
import DB (DBConfig (..), withDB)
import qualified Logging as Logging
import qualified Shelley as Shelley

data ByronProxyOptions = ByronProxyOptions
  { bpoDatabasePath                :: !FilePath
  , bpoIndexPath                   :: !FilePath
  , bpoLoggerConfigPath            :: !(Maybe FilePath)
    -- ^ Optional file path; will use default configuration if none given.
  , bpoCardanoConfigurationOptions :: !CSL.ConfigurationOptions
  , bpoShelleyOptions              :: !ShelleyOptions
  , bpoByronOptions                :: !(Maybe ByronOptions)
  }

-- | Host and port on which to run the Shelley server.
data ShelleyOptions = ShelleyOptions
  { soLocalAddress      :: !Address
  , soProducerAddresses :: ![Address]
  }

data Address = Address
  { hostName    :: !Network.HostName
  , serviceName :: !Network.ServiceName
  }

resolveAddress :: Address -> IO Network.AddrInfo
resolveAddress addr = do
  addrInfos <- Network.getAddrInfo
    (Just Network.defaultHints)
    (Just (hostName addr))
    (Just (serviceName addr))
  case addrInfos of
    [] -> error "no getAddrInfo"
    (addrInfo : _) -> pure addrInfo

data ByronOptions = ByronOptions
  { boNetworkOptions :: !CSL.NetworkConfigOpts
    -- ^ To use with `intNetworkConfigOpts` to get a `NetworkConfig` from
    -- cardano-sl, required in order to run a diffusion layer.
  }

-- | `Tracer` comes from the `contra-tracer` package, but cardano-sl still
-- works with the cardano-sl-util definition of the same thing.
mkCSLTrace
  :: Tracer IO (Monitoring.LoggerName, Monitoring.Severity, Text.Builder)
  -> Trace IO (Trace.LogNamed (Wlog.Severity, Text))
mkCSLTrace tracer = case tracer of
  Tracer f -> Pos.Util.Trace.Trace $ Op $ \namedI ->
    let logName    = Text.intercalate (Text.pack ".") (Trace.lnName namedI)
        (sev, txt) = Trace.lnItem namedI
    in  f (logName, convertSeverity sev, Text.fromText txt)

  where

  -- | Diffusion layer uses log-waper severity, iohk-monitoring uses its own.
  convertSeverity :: Wlog.Severity -> Monitoring.Severity
  convertSeverity sev = case sev of
    Wlog.Debug   -> Monitoring.Debug
    Wlog.Info    -> Monitoring.Info
    Wlog.Notice  -> Monitoring.Notice
    Wlog.Warning -> Monitoring.Warning
    Wlog.Error   -> Monitoring.Error

-- | Parser for command line options.
cliParser :: Opt.Parser ByronProxyOptions
cliParser = ByronProxyOptions
  <$> cliDatabasePath
  <*> cliIndexPath
  <*> cliLoggerConfigPath
  <*> cliCardanoConfigurationOptions
  <*> cliShelleyOptions
  <*> cliByronOptions

  where

  cliDatabasePath = Opt.strOption $
    Opt.long "database-path"   <>
    Opt.metavar "FILEPATH"     <>
    Opt.value "db-byron-proxy" <>
    Opt.help "Path to folder of the database. Will be created if it does not exist."

  cliIndexPath = Opt.strOption $
    Opt.long "index-path"         <>
    Opt.metavar "FILEPATH"        <>
    Opt.value "index-byron-proxy" <>
    Opt.help "Path to folder of the SQLite database index."

  cliLoggerConfigPath = Opt.optional $ Opt.strOption $
    Opt.long "logger-config" <>
    Opt.metavar "FILEPATH"   <>
    Opt.help "Path to the logger config file."

  cliCardanoConfigurationOptions = CSL.configurationOptionsParser

  cliShelleyOptions = ShelleyOptions
    <$> cliAddress "local" "local address"
    <*> Opt.many (cliAddress "producer" "producer address")

  cliAddress :: String -> String -> Opt.Parser Address
  cliAddress prefix help = fmap (uncurry Address) $ Opt.option Opt.auto $
    Opt.long (dashconcat (prefix : ["addr"])) <>
    Opt.metavar "(HOSTNAME,SERVIVCENAME)" <>
    Opt.help help

  {-
  cliHostName prefix = Opt.strOption $
    Opt.long (dashconcat (prefix ++ ["host"])) <>
    Opt.metavar "HOST" <>
    Opt.help "Host"

  cliServiceName prefix = Opt.strOption $
    Opt.long (dashconcat (prefix ++ ["port"])) <>
    Opt.metavar "PORT" <>
    Opt.help "Port"
  -}

  cliByronOptions :: Opt.Parser (Maybe ByronOptions)
  cliByronOptions = Opt.optional $
    ByronOptions <$> cliNetworkConfig

  -- We can't use `Pos.Infra.Network.CLI.networkConfigOption` from
  -- cardano-sl-infra because all of its fields are optional, but we need at
  -- least one non-optional, so that this parser can fail, and therefore
  -- `cliClientOptions` can give `Nothing.
  -- Very similar to `Pos.Infra.Network.CLI.networkConfigOption`, but the
  -- topology file is _not_ optional.
  cliNetworkConfig :: Opt.Parser CSL.NetworkConfigOpts
  cliNetworkConfig = do
      ncoTopology <-
          fmap Just $
          Opt.strOption $
          mconcat
              [ Opt.long "topology"
              , Opt.metavar "FILEPATH"
              , Opt.help "Path to a YAML file containing the network topology"
              ]
      ncoKademlia <-
          Opt.optional $ Opt.strOption $
          mconcat
              [ Opt.long "kademlia"
              , Opt.metavar "FILEPATH"
              , Opt.help
                    "Path to a YAML file containing the kademlia configuration"
              ]
      ncoSelf <-
          Opt.optional $ Opt.option (fromString <$> Opt.str) $
          mconcat
              [ Opt.long "node-id"
              , Opt.metavar "NODE_ID"
              , Opt.help "Identifier for this node within the network"
              ]
      ncoPort <-
          Opt.option Opt.auto $
          mconcat
              [ Opt.long "default-port"
              , Opt.metavar "PORT"
              , Opt.help "Port number for IP address to node ID translation"
              , Opt.value 3000
              ]
      ncoPolicies <-
          Opt.optional $ Opt.strOption $
          mconcat
              [ Opt.long "policies"
              , Opt.metavar "FILEPATH"
              , Opt.help "Path to a YAML file containing the network policies"
              ]
      ncoExternalAddress <- Opt.optional $ CSL.externalNetworkAddressOption Nothing
      ncoBindAddress <- Opt.optional $ CSL.listenNetworkAddressOption Nothing
      pure $ CSL.NetworkConfigOpts
        { CSL.ncoTopology = ncoTopology
        , CSL.ncoKademlia = ncoKademlia
        , CSL.ncoSelf = ncoSelf
        , CSL.ncoPort = ncoPort
        , CSL.ncoPolicies = ncoPolicies
        , CSL.ncoExternalAddress = ncoExternalAddress
        , CSL.ncoBindAddress = ncoBindAddress
        }

  dashconcat :: [String] -> String
  dashconcat = intercalate "-"

-- | Parser "info" for command line options (optparse-applicative).
cliParserInfo :: Opt.ParserInfo ByronProxyOptions
cliParserInfo = Opt.info cliParser infoMod
  where
  infoMod :: Opt.InfoMod ByronProxyOptions
  infoMod =
       Opt.header "Byron proxy"
    <> Opt.progDesc "Store and forward blocks from a Byron or Shelley server"
    <> Opt.fullDesc

runShelleyServer
  :: ( )
  => Address
  -> ThreadRegistry IO
  -> ConnectionTable IO
  -> Shelley.ResponderVersions
  -> IO ()
runShelleyServer addr _ ctable rversions = do
  addrInfo <- resolveAddress addr
  withServer
    ctable
    addrInfo
    Shelley.mkPeer
    (\(DictVersion _) -> acceptEq)
    rversions
    wait

runShelleyClient
  :: ( )
  => [Address]
  -> ThreadRegistry IO
  -> ConnectionTable IO
  -> Shelley.InitiatorVersions
  -> IO ()
runShelleyClient producerAddrs _ ctable iversions = do
  -- Expect AddrInfo, convert to SockAddr, then pass to ipSubscriptionWorker
  sockAddrs <- mapM resolveAddress producerAddrs
  ipSubscriptionWorker
    ctable
    nullTracer
    (Just (Network.SockAddrInet 0 0))
    (Just (Network.SockAddrInet6 0 0 (0, 0, 0, 1) 0))
    (const Nothing)
    (IPSubscriptionTarget {
         ispIps     = fmap Network.addrAddress sockAddrs
       , ispValency = length sockAddrs
       })
    (\sock -> do
        connectToNode'
          (\(DictVersion codec) -> encodeTerm codec)
          (\(DictVersion codec) -> decodeTerm codec)
          Shelley.mkPeer
          (iversions)
          sock
    )
    wait

runByron
  :: ( CSL.HasConfigurations, ByronGiven )
  => Tracer IO (Monitoring.LoggerName, Monitoring.Severity, Text.Builder)
  -> ByronOptions
  -> CSL.Genesis.Config
  -> Cardano.ProtocolMagicId
  -> Cardano.EpochSlots
  -> Index IO (Header (Block ByronConfig))
  -> ChainDB IO (Block ByronConfig)
  -> IO ()
runByron tracer byronOptions genesisConfig _ epochSlots idx db = do
    let cslTrace = mkCSLTrace tracer
    -- Get the `NetworkConfig` from the options
    networkConfig <- CSL.intNetworkConfigOpts
      (Trace.named cslTrace)
      (boNetworkOptions byronOptions)
    let bpc :: ByronProxyConfig
        bpc = ByronProxyConfig
          { bpcAdoptedBVData = CSL.configBlockVersionData genesisConfig
            -- ^ Hopefully that never needs to change.
          , bpcEpochSlots = epochSlots
          , bpcNetworkConfig = networkConfig
              { ncEnqueuePolicy = Policy.defaultEnqueuePolicyRelay
              , ncDequeuePolicy = Policy.defaultDequeuePolicyRelay
              }
            -- ^ These default relay policies should do what we want.
            -- If not, could give a --policy option and use yaml files as in
            -- cardano-sl
          , bpcDiffusionConfig = CSL.FullDiffusionConfiguration
              { CSL.fdcProtocolMagic = CSL.configProtocolMagic genesisConfig
              , CSL.fdcProtocolConstants = CSL.configProtocolConstants genesisConfig
              , CSL.fdcRecoveryHeadersMessage = CSL.recoveryHeadersMessage
              , CSL.fdcLastKnownBlockVersion = CSL.lastKnownBlockVersion CSL.updateConfiguration
              , CSL.fdcConvEstablishTimeout = CSL.networkConnectionTimeout
              -- Diffusion layer logs will have "diffusion" in their names.
              , CSL.fdcTrace = Trace.appendName "diffusion" cslTrace
              , CSL.fdcStreamWindow = CSL.streamWindow
              , CSL.fdcBatchSize    = 64
              }
            -- 40 seconds.
            -- TODO configurable for these 3.
          , bpcPoolRoundInterval = 40000000
          , bpcSendQueueSize     = 1
          , bpcRecvQueueSize     = 1
          }
        genesisBlock = CSL.genesisBlock0 (CSL.configProtocolMagic genesisConfig)
                                         (CSL.configGenesisHash genesisConfig)
                                         (CSL.genesisLeaders genesisConfig)
    withByronProxy (contramap (\(a, b) -> ("", a, b)) tracer) bpc idx db $ \bp ->
      byronClient genesisBlock bp

  where

  byronClient genesisBlock bp = void $ concurrently
    (Byron.download textTracer genesisBlock epochSlots db bp k)
    (Byron.announce Nothing                            db bp)
    where
    k _ _ = pure ()

  textTracer :: Tracer IO Text.Builder
  textTracer = contramap
    (\tbuilder -> (Text.pack "main.client", Monitoring.Info, tbuilder))
    tracer

main :: IO ()
main = do
  bpo <- Opt.execParser cliParserInfo
  Logging.withLogging (bpoLoggerConfigPath bpo) "byron-proxy" $ \trace -> do
    -- We always need the cardano-sl configuration, even if we're not
    -- connecting to a Byron peer, because that's where the blockchain
    -- configuration comes from: slots-per-epoch in particular.
    -- We'll use the tracer that was just set up to give debug output. That
    -- requires converting the iohk-monitoring trace to the one used in CSL.
    let confOpts = bpoCardanoConfigurationOptions bpo
    -- Legacy cardano-sl genesis configuration is used. Then, it's converted
    -- to the new cardano-ledger style.
    yamlConfig <- CSL.parseYamlConfig (CSL.cfoFilePath confOpts) (CSL.cfoKey confOpts)
    let configDir = takeDirectory $ CSL.cfoFilePath confOpts
    -- Old part.
    oldGenesisConfig <- CSL.mkConfigFromStaticConfig
      configDir
      (CSL.cfoSystemStart confOpts)
      (CSL.cfoSeed confOpts)
      (CSL.ccReqNetMagic yamlConfig)
      (CSL.ccTxValRules yamlConfig)
      (CSL.ccGenesis yamlConfig)
    -- New part.
    -- Uses a MonadError constraint, so use runExceptT and throw an
    -- exception if it fails (the configuration error is not an Exception
    -- though, so userError is used).
    outcome <- runExceptT $ Cardano.mkConfigFromStaticConfig
      (convertRequiresNetworkMagic (CSL.ccReqNetMagic yamlConfig))
      (fmap convertSystemStart (CSL.cfoSystemStart confOpts))
      (CSL.cfoSeed confOpts)
      -- Careful to adjust the file path: CSL puts the configuration directory
      -- part (`configDir`) in front of it, but cardano-ledger's variant does
      -- not.
      (convertStaticConfig configDir (CSL.ccGenesis yamlConfig))
    newGenesisConfig <- case outcome of
      Left confError -> throwIO (userError (show confError))
      Right it       -> pure it
    let epochSlots = Cardano.EpochSlots (fromIntegral (CSL.configEpochSlots oldGenesisConfig))
        protocolMagic = Cardano.ProtocolMagicId
          . fromIntegral -- Int32 -> Word32
          . CSL.unProtocolMagicId
          . CSL.getProtocolMagicId
          . CSL.configProtocolMagic
          $ oldGenesisConfig
        -- Next, set up the database, taking care to seed with the genesis
        -- block if it's empty.
        dbc :: DBConfig
        dbc = DBConfig
          { dbFilePath    = bpoDatabasePath bpo
          , indexFilePath = bpoIndexPath bpo
          }
    -- Fulfill ByronGiven, and the necessary cardano-sl reflection configurations.
    CSL.withUpdateConfiguration (CSL.ccUpdate yamlConfig) $
      CSL.withSscConfiguration (CSL.ccSsc yamlConfig) $
      CSL.withBlockConfiguration (CSL.ccBlock yamlConfig) $
      CSL.withDlgConfiguration (CSL.ccDlg yamlConfig) $
      CSL.withNodeConfiguration (CSL.ccNode yamlConfig) $
      Reflection.give protocolMagic $ Reflection.give epochSlots $ do
        -- Trace DB writes in such a way that they appear in EKG.
        -- FIXME surprisingly, contra-tracer doesn't give a way to do this.
        -- It should export
        --
        --   Applicative m => (a -> Maybe b) -> Tracer m a -> Tracer m b
        --
        -- or similar
        --
        --
        -- This is defined here because we need the reflection instances
        -- for ByronGiven.
        let Tracer doConvertedTrace = Logging.convertTrace trace
            dbTracer :: Tracer IO (TraceEvent (Block ByronConfig))
            dbTracer = Tracer $ \trEvent -> case trEvent of
              TraceAddBlockEvent (AddedBlockToVolDB point) -> case point of
                Point Origin -> pure ()
                Point (At (Point.Block (SlotNo slotno) _)) ->
                  -- NB this is here because devops wanted an EKG metric on
                  -- block count. FIXME should be done in a more sane way...
                  let val = ("db", Monitoring.Info, Monitoring.LogValue "block count" (Monitoring.PureI (fromIntegral slotno)))
                  in  doConvertedTrace val
              TraceAddBlockEvent (AddBlockValidation it@(InvalidBlock _ _)) ->
                  let val = ("db", Monitoring.Error, Monitoring.LogMessage (fromString (show it)))
                  in  doConvertedTrace val >> error ("oops: " ++ show it)
              _ -> pure ()
            indexTracer :: Tracer IO Index.TraceEvent
            indexTracer = Tracer $ \trEvent ->
              let val = ("index", Monitoring.Info, Monitoring.LogMessage (fromString (show trEvent)))
              in  doConvertedTrace val
        traceWith (Logging.convertTrace' trace) ("", Monitoring.Info, fromString "Opening database")
        -- Thread registry is needed by ChainDB and by the network protocols.
        -- I assume it's supposed to be shared?
        withThreadRegistry $ \treg -> do
          let -- TODO Grab this from the newGenesisConfig config
              securityParam = SecurityParam 2160
              protocolVersion = Cardano.ProtocolVersion 1 0 0
              softwareVersion = Cardano.SoftwareVersion
                (Cardano.ApplicationName (fromString "cardano-byron-proxy")) 2
              protocolInfo = protocolInfoByron
                newGenesisConfig
                Nothing -- Default signature threshold.
                protocolVersion
                softwareVersion
                Nothing
              -- We need to use NodeConfig for the ChainDB but _also_ to get
              -- the network protocols.
              nodeConfig = pInfoConfig protocolInfo
              nodeState = pInfoInitState protocolInfo
              extLedgerState = pInfoInitLedger protocolInfo
              slotMs = Cardano.ppSlotDuration (Cardano.gdProtocolParameters (Cardano.configGenesisData newGenesisConfig))
              slotDuration = SlotLength (fromRational (toRational slotMs / 1000))
              systemStart = SystemStart (Cardano.gdStartTime (Cardano.configGenesisData newGenesisConfig))
          btime <- realBlockchainTime treg slotDuration systemStart
          withDB dbc dbTracer indexTracer treg securityParam nodeConfig extLedgerState $ \idx cdb -> do
            traceWith (Logging.convertTrace' trace) ("", Monitoring.Info, fromString "Database opened")
            Shelley.withVersions treg cdb nodeConfig nodeState btime $ \ctable iversions rversions -> do
              let server = runShelleyServer (soLocalAddress      (bpoShelleyOptions bpo)) treg ctable rversions
                  client = runShelleyClient (soProducerAddresses (bpoShelleyOptions bpo)) treg ctable iversions
                  byron  = case bpoByronOptions bpo of
                    Nothing -> pure ()
                    Just bopts -> runByron
                      (Logging.convertTrace' trace)
                      bopts
                      oldGenesisConfig
                      protocolMagic
                      epochSlots
                      idx
                      cdb
              _ <- concurrently (concurrently server client) byron
              pure ()
