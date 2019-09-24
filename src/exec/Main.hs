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
import Control.Concurrent.Async (concurrently, link, wait, withAsync)
import Control.Exception (throwIO)
import Control.Monad (mapM, void)
import Control.Monad.STM (retry)
import Control.Monad.Trans.Except (runExceptT)
import Control.Tracer (Tracer (..), contramap, nullTracer, traceWith)
import Data.Functor.Contravariant (Op (..))
import Data.List (intercalate)
import qualified Data.Reflection as Reflection (give, given)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Text
import qualified Network.Socket as Network
import qualified Options.Applicative as Opt
import System.FilePath ((</>), takeDirectory)
import System.IO.Error (userError)

import qualified Cardano.BM.Data.Aggregated as Monitoring
import qualified Cardano.BM.Data.LogItem as Monitoring
import qualified Cardano.BM.Data.Severity as Monitoring

import qualified Cardano.Chain.Genesis as Cardano
import qualified Cardano.Chain.Slotting as Cardano
import qualified Cardano.Chain.Update as Cardano
import qualified Cardano.Crypto as Cardano

import           Network.Broadcast.OutboundQueue (ReconsiderAfter (..))

import qualified Pos.Chain.Block as CSL (genesisBlock0)
import qualified Pos.Chain.Block as CSL (BlockConfiguration, withBlockConfiguration)
import qualified Pos.Chain.Delegation as CSL
import qualified Pos.Chain.Lrc as CSL (genesisLeaders)
import qualified Pos.Chain.Genesis as CSL.Genesis (Config)
import qualified Pos.Chain.Genesis as CSL
import qualified Pos.Chain.Update as CSL
import qualified Pos.Chain.Ssc as CSL (withSscConfiguration)
import qualified Pos.Configuration as CSL (NodeConfiguration, withNodeConfiguration)
import qualified Pos.Crypto as CSL

import qualified Pos.Client.CLI.Options as CSL (configurationOptionsParser)
import qualified Pos.Infra.Network.CLI as CSL (NetworkConfigOpts (..),
                                               externalNetworkAddressOption,
                                               intNetworkConfigOpts,
                                               launchStaticConfigMonitoring,
                                               listenNetworkAddressOption)
import Pos.Infra.Network.Types (NetworkConfig (..))
import qualified Pos.Infra.Network.Policy as Policy
import qualified Pos.Launcher.Configuration as CSL (Configuration (..),
                                                    ConfigurationOptions (..))
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
import Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..), protocolSecurityParam)
import Ouroboros.Consensus.Mempool.API (Mempool)
import Ouroboros.Consensus.Mempool.TxSeq (TicketNo)
import Ouroboros.Consensus.Node (getMempool)
import Ouroboros.Consensus.Node.ProtocolInfo.Abstract (ProtocolInfo (..))
import Ouroboros.Consensus.Node.ProtocolInfo.Byron (PBftSignatureThreshold (..),
           protocolInfoByron)
import Ouroboros.Network.NodeToNode (withServer)
import Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import qualified Ouroboros.Consensus.Util.ResourceRegistry as ResourceRegistry (withRegistry)
import Ouroboros.Network.Block (SlotNo (..), Point (..))
import Ouroboros.Network.Point (WithOrigin (..))
import qualified Ouroboros.Network.Point as Point (Block (..))
import Ouroboros.Network.Protocol.Handshake.Type (acceptEq)
import Ouroboros.Network.Protocol.Handshake.Version (DictVersion (..))
import Ouroboros.Network.Server.ConnectionTable (ConnectionTable)
import Ouroboros.Network.Socket (connectToNode')
import Ouroboros.Network.Subscription (IPSubscriptionTarget (..), ipSubscriptionWorker)
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
  , bpoPBftSignatureThreshold      :: !(Maybe PBftSignatureThreshold)
    -- ^ PBFT signature threshold parameter. It's not present in, and cannot
    -- be derived from, the cardano-sl configuration.
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

-- | Simple address parser. Host goes in square brackets, followed by a colon
-- and a non-empty service name
--
--   [127.0.0.1]:1024
--
parseAddress :: String -> Either String Address
parseAddress str = case between '[' ']' str of
    Nothing -> Left "Expected a host name between square brackets [hostname]"
    Just (host, str') -> case str' of
      ':' : service@(_ : _) -> Right $ Address host service
      _                     -> Left "Expected a colon followed by service name [hostname]:service"
  where
    between :: Char -> Char -> String -> Maybe (String, String)
    between _     _   [] = Nothing
    between start end (s : ss) =
      if start == s
      then untilEnd end ss []
      else Nothing
    untilEnd :: Char -> String -> String -> Maybe (String, String)
    untilEnd _   []       _   = Nothing
    untilEnd end (s : ss) acc =
      if end == s
      then Just (reverse acc, ss)
      else untilEnd end ss (s : acc)

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
  <*> cliPBftSignatureThreshold

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
  cliAddress prefix help = Opt.option (Opt.eitherReader parseAddress) $
    Opt.long (dashconcat (prefix : ["addr"])) <>
    Opt.metavar "[HOSTNAME]:SERVIVCENAME" <>
    Opt.help help

  -- Uses the Read instance for Double.
  cliPBftSignatureThreshold :: Opt.Parser (Maybe PBftSignatureThreshold)
  cliPBftSignatureThreshold = Opt.optional $ Opt.option (PBftSignatureThreshold <$> Opt.auto) $
    Opt.long "pbft-threshold" <>
    Opt.metavar "DOUBLE" <>
    Opt.help "PBft signature threshold"

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
  -> ResourceRegistry IO
  -> ConnectionTable IO Network.SockAddr
  -> Shelley.ResponderVersions
  -> IO ()
runShelleyServer addr _ ctable rversions = do
  addrInfo <- resolveAddress addr
  withServer
    nullTracer
    ctable
    addrInfo
    Shelley.mkPeer
    (\(DictVersion _) -> acceptEq)
    rversions
    wait

runShelleyClient
  :: ( )
  => [Address]
  -> ResourceRegistry IO
  -> ConnectionTable IO Network.SockAddr
  -> Shelley.InitiatorVersions
  -> IO ()
runShelleyClient producerAddrs _ ctable iversions = do
  -- Expect AddrInfo, convert to SockAddr, then pass to ipSubscriptionWorker
  sockAddrs <- mapM resolveAddress producerAddrs
  ipSubscriptionWorker
    nullTracer
    ctable
    (Just (Network.SockAddrInet 0 0))
    (Just (Network.SockAddrInet6 0 0 (0, 0, 0, 1) 0))
    (const Nothing)
    (IPSubscriptionTarget {
         ispIps     = fmap Network.addrAddress sockAddrs
       , ispValency = length sockAddrs
       })
    (const retry)
    (\sock -> do
        connectToNode'
          (\(DictVersion codec) -> encodeTerm codec)
          (\(DictVersion codec) -> decodeTerm codec)
          nullTracer
          Shelley.mkPeer
          (iversions)
          sock
    )

runByron
  :: ( ByronGiven )
  => Tracer IO (Monitoring.LoggerName, Monitoring.Severity, Text.Builder)
  -> ByronOptions
  -> CSL.Genesis.Config
  -> CSL.BlockConfiguration
  -> CSL.UpdateConfiguration
  -> CSL.NodeConfiguration
  -> Cardano.EpochSlots
  -> SecurityParam
  -> Index IO (Header (Block ByronConfig))
  -> ChainDB IO (Block ByronConfig)
  -> Mempool IO (Block ByronConfig) TicketNo
  -> IO ()
runByron tracer byronOptions genesisConfig blockConfig updateConfig nodeConfig epochSlots k idx db mempool = do
    let cslTrace = mkCSLTrace tracer
        trace = Trace.appendName "diffusion" cslTrace
    -- Get the `NetworkConfig` from the options
    networkConfig <- CSL.intNetworkConfigOpts
      (Trace.named cslTrace)
      (boNetworkOptions byronOptions)
    let networkConfig' = networkConfig
          { ncEnqueuePolicy = Policy.defaultEnqueuePolicyRelay
          , ncDequeuePolicy = Policy.defaultDequeuePolicyRelay
          -- In cardano-sl, the outbound queue's failure policy controls
          -- whether a message is enqueued to a peer. It's set explicitly here
          -- for clarity, but it's the same as the default.
          , ncFailurePolicy = \_ _ _ -> ReconsiderAfter 200
          }
        bpc :: ByronProxyConfig
        bpc = configFromCSLConfigs
                genesisConfig
                blockConfig
                updateConfig
                nodeConfig
                networkConfig'
                64 -- Batch size.
                trace
        genesisBlock = CSL.genesisBlock0 (CSL.configProtocolMagic genesisConfig)
                                         (CSL.configGenesisHash genesisConfig)
                                         (CSL.genesisLeaders genesisConfig)
    -- *MUST* launch static config monitoring thread, otherwise nodes
    -- configured with static routes *will not update their peers lists*.
    -- It would be much better if intNetworkConfigOpts set up the static
    -- peers to begin with, but that's just not how it is.
    withAsync (CSL.launchStaticConfigMonitoring (ncTopology networkConfig)) $ \monitorThread ->
      withByronProxy (contramap (\(a, b) -> ("", a, b)) tracer) bpc idx db mempool $ \bp -> do
        link monitorThread
        byronClient genesisBlock bp

  where

  byronClient genesisBlock bp = void $ concurrently
    (Byron.download textTracer genesisBlock epochSlots k db bp)
    (Byron.announce Nothing                              db bp)

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
    newGenesisConfig <- case CSL.ccGenesis yamlConfig of
      -- Recently, making a config from a "spec" was removed from cardano-ledger,
      -- so if the CSL configuration does not use a genesis json file then
      -- we just give up.
      CSL.GCSpec _ -> throwIO (userError "genesis config from spec not supported, must use a json file")
      CSL.GCSrc fp hash -> do
        -- Uses a MonadError constraint, so use runExceptT and throw an
        -- exception if it fails (the configuration error is not an Exception
        -- though, so userError is used).
        outcome <- runExceptT $ Cardano.mkConfigFromFile
          (convertRequiresNetworkMagic (CSL.ccReqNetMagic yamlConfig))
          -- Careful to adjust the file path: CSL puts the configuration directory
          -- part (`configDir`) in front of it, but cardano-ledger's variant does
          -- not.
          (configDir </> fp)
          (convertHash hash)
        case outcome of
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
        ResourceRegistry.withRegistry $ \rr -> do
          let protocolVersion = Cardano.ProtocolVersion 1 0 0
              softwareVersion = Cardano.SoftwareVersion
                (Cardano.ApplicationName (fromString "cardano-byron-proxy")) 2
              protocolInfo = protocolInfoByron
                newGenesisConfig
                (bpoPBftSignatureThreshold bpo)
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
          btime <- realBlockchainTime rr slotDuration systemStart
          withDB dbc dbTracer indexTracer rr nodeConfig extLedgerState $ \idx cdb -> do
            traceWith (Logging.convertTrace' trace) ("", Monitoring.Info, fromString "Database opened")
            Shelley.withShelley rr cdb nodeConfig nodeState btime $ \kernel ctable iversions rversions -> do
              let server = runShelleyServer (soLocalAddress      (bpoShelleyOptions bpo)) rr ctable rversions
                  client = runShelleyClient (soProducerAddresses (bpoShelleyOptions bpo)) rr ctable iversions
                  byron  = case bpoByronOptions bpo of
                    Nothing -> pure ()
                    Just bopts -> runByron
                      (Logging.convertTrace' trace)
                      bopts
                      oldGenesisConfig
                      (Reflection.given :: CSL.BlockConfiguration)
                      (Reflection.given :: CSL.UpdateConfiguration)
                      (Reflection.given :: CSL.NodeConfiguration)
                      epochSlots
                      (protocolSecurityParam nodeConfig)
                      idx
                      cdb
                      (getMempool kernel)
              _ <- concurrently (concurrently server client) byron
              pure ()
