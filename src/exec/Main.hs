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

import Control.Concurrent.Async (concurrently, link, withAsync)
import Control.Exception (IOException, throwIO)
import Control.Monad (mapM, void)
import Control.Monad.Trans.Except (runExceptT)
import Control.Tracer (Tracer (..), contramap, nullTracer, traceWith)
import Data.Functor.Contravariant (Op (..))
import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import qualified Data.Reflection as Reflection (given)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Text
import Data.Time (DiffTime)
import Data.Void (Void)
import qualified Network.Socket as Network
import qualified Options.Applicative as Opt
import System.FilePath ((</>), takeDirectory)
import System.IO.Error (userError)

import Codec.CBOR.Read (DeserialiseFailure)

import qualified Cardano.BM.Data.Aggregated as Monitoring
import qualified Cardano.BM.Data.LogItem as Monitoring
import qualified Cardano.BM.Data.Severity as Monitoring

import qualified Cardano.Chain.Genesis as Cardano
import qualified Cardano.Chain.Slotting as Cardano
import qualified Cardano.Chain.Update as Cardano

import qualified Pos.Chain.Block as CSL (genesisBlock0)
import qualified Pos.Chain.Block as CSL (BlockConfiguration, withBlockConfiguration)
import qualified Pos.Chain.Delegation as CSL
import qualified Pos.Chain.Lrc as CSL (genesisLeaders)
import qualified Pos.Chain.Genesis as CSL.Genesis (Config)
import qualified Pos.Chain.Genesis as CSL
import qualified Pos.Chain.Update as CSL
import qualified Pos.Chain.Ssc as CSL (withSscConfiguration)
import qualified Pos.Configuration as CSL (NodeConfiguration, withNodeConfiguration)

import qualified Pos.Infra.Network.CLI as CSL (NetworkConfigOpts (..),
                                               externalNetworkAddressOption,
                                               intNetworkConfigOpts,
                                               launchStaticConfigMonitoring,
                                               listenNetworkAddressOption)
import Pos.Infra.Network.Types (NetworkConfig (..))
import qualified Pos.Infra.Network.Policy as Policy
import qualified Pos.Launcher.Configuration as CSL (Configuration (..),
                                                    ConfigurationOptions (..))
import qualified Pos.Client.CLI.Options as CSL (configurationOptionsParser)
import qualified Pos.Util.Config as CSL (parseYamlConfig)

import Pos.Util.Trace (Trace)
import qualified Pos.Util.Trace.Named as Trace (LogNamed (..), appendName, named)
import qualified Pos.Util.Trace
import qualified Pos.Util.Wlog as Wlog

import Network.Mux.Trace (MuxError (..), MuxErrorType (..))

import Ouroboros.Byron.Proxy.Block (ByronBlock)
import Ouroboros.Byron.Proxy.Index.Types (Index)
import qualified Ouroboros.Byron.Proxy.Index.Sqlite as Index (TraceEvent)
import Ouroboros.Byron.Proxy.Genesis.Convert
import Ouroboros.Byron.Proxy.Main
import Ouroboros.Consensus.Block (GetHeader (Header))
import Ouroboros.Consensus.BlockchainTime (SlotLength (..),
                                           SystemStart (..),
                                           focusSlotLengths,
                                           realBlockchainTime,
                                           singletonSlotLengths)
import Ouroboros.Consensus.Config (configSecurityParam)
import Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import Ouroboros.Consensus.Mempool.API (Mempool)
import Ouroboros.Consensus.Mempool.TxSeq (TicketNo)
import Ouroboros.Consensus.Node (getMempool)
import Ouroboros.Consensus.Node.ErrorPolicy (consensusErrorPolicy)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Ouroboros.Consensus.Node.Run ()
import Ouroboros.Consensus.Byron.Node (PBftSignatureThreshold (..), protocolInfoByron)
import Ouroboros.Network.NodeToNode
import Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import qualified Ouroboros.Consensus.Util.ResourceRegistry as ResourceRegistry (withRegistry)
import Ouroboros.Network.Block (BlockNo (..))
import Ouroboros.Network.ErrorPolicy (WithAddr (..), ErrorPolicyTrace (..))
import Ouroboros.Network.ErrorPolicy (SuspendDecision (..))
import Ouroboros.Network.BlockFetch.Client (BlockFetchProtocolFailure)
import Ouroboros.Network.Protocol.Handshake.Type (HandshakeClientProtocolError)
import qualified Ouroboros.Network.TxSubmission.Inbound as TxInbound
import qualified Ouroboros.Network.TxSubmission.Outbound as TxOutbound
import Ouroboros.Network.Server.ConnectionTable (ConnectionTable)
import Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import Ouroboros.Consensus.Storage.ChainDB.Impl.Types (TraceEvent (..), TraceAddBlockEvent (..), TraceValidationEvent (..))

import Ouroboros.Network.IOManager (AssociateWithIOCP, withIOManager)
import Ouroboros.Network.Snocket (socketSnocket)

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
      ncoCheckPeerHost <-
          (not <$>) .
          Opt.switch $
          mconcat
              [ Opt.long "disable-peer-host-check"
              , Opt.help "DANGER: disable the peer host address consistency check. Makes your node vulnerable"
              ]
      ncoExternalAddress <- Opt.optional $ CSL.externalNetworkAddressOption Nothing
      ncoBindAddress <- Opt.optional $ CSL.listenNetworkAddressOption Nothing
      pure $ CSL.NetworkConfigOpts
        { CSL.ncoTopology = ncoTopology
        , CSL.ncoSelf = ncoSelf
        , CSL.ncoPort = ncoPort
        , CSL.ncoPolicies = ncoPolicies
        , CSL.ncoExternalAddress = ncoExternalAddress
        , CSL.ncoBindAddress = ncoBindAddress
        , CSL.ncoCheckPeerHost = ncoCheckPeerHost
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
  :: AssociateWithIOCP
  -> Tracer IO (WithAddr Network.SockAddr ErrorPolicyTrace)
  -> Address
  -> ResourceRegistry IO
  -> ConnectionTable IO Network.SockAddr
  -> Shelley.ResponderVersions
  -> IO Void
runShelleyServer iocp tracer addr _ _ rversions = do
  addrInfo <- resolveAddress addr
  networkState <- newNetworkMutableState
  withServer
    (socketSnocket iocp)
    NetworkServerTracers {
        nstMuxTracer = nullTracer,
        nstHandshakeTracer = nullTracer,
        nstErrorPolicyTracer = tracer
    }
    networkState
    (Network.addrAddress addrInfo)
    rversions
    errorPolicy
  where
    -- TODO: We might need some additional proxy-specific policies
    errorPolicy :: ErrorPolicies
    errorPolicy = networkErrorPolicy <> consensusErrorPolicy (Proxy :: Proxy ByronBlock)


runShelleyClient
  :: AssociateWithIOCP
  -> Tracer IO (WithAddr Network.SockAddr ErrorPolicyTrace)
  -> [Address]
  -> ResourceRegistry IO
  -> ConnectionTable IO Network.SockAddr
  -> Shelley.InitiatorVersions
  -> IO Void
runShelleyClient iocp tracer producerAddrs _ _ iversions = do
  -- Expect AddrInfo, convert to SockAddr, then pass to ipSubscriptionWorker
  sockAddrs <- mapM resolveAddress producerAddrs
  networkState <- newNetworkMutableState -- TODO: Ideally should share state with runShelleyServer
  ipSubscriptionWorker
    (socketSnocket iocp)
    NetworkSubscriptionTracers {
        nsMuxTracer          = nullTracer,
        nsHandshakeTracer    = nullTracer,
        nsErrorPolicyTracer  = tracer,
        nsSubscriptionTracer = nullTracer
    }
    networkState
    SubscriptionParams {
      spLocalAddresses = (LocalAddresses
        (Just (Network.SockAddrInet 0 0))
        (Just (Network.SockAddrInet6 0 0 (0, 0, 0, 1) 0))
        Nothing),
      spConnectionAttemptDelay = const Nothing,
      spErrorPolicies = errorPolicy,
      spSubscriptionTarget = (IPSubscriptionTarget {
          ispIps     = fmap Network.addrAddress sockAddrs
        , ispValency = length sockAddrs
       })
    }
    iversions
  where
    -- TODO: We might need some additional proxy-specific policies
        errorPolicy = networkErrorPolicy <> consensusErrorPolicy (Proxy :: Proxy ByronBlock)

-- | Error policies for a trusted deployment (behind a firewall).
--

networkErrorPolicy :: ErrorPolicies
networkErrorPolicy = ErrorPolicies {
    epAppErrorPolicies = [
        -- Handshake client protocol error: we either did not recognise received
        -- version or we refused it.  This is only for outbound connections,
        -- thus we suspend the consumer.
        ErrorPolicy
          $ \(_ :: HandshakeClientProtocolError NodeToNodeVersion)
                -> Just misconfiguredPeer

        -- exception thrown by `runDecoderWithByteLimit`; we received to
        -- many bytes from the network.
--      , ErrorPolicy
--          $ \(_ :: DecoderFailureOrTooMuchInput DeserialiseFailure)
--                 -> Just theyBuggy

        -- deserialisation failure;  It means that the remote peer has
        -- a bug.
      , ErrorPolicy
          $ \(_ :: DeserialiseFailure)
                -> Just theyBuggy

        -- network errors:
        -- * if the bearer sends invalid data we suspend the peer for
        --   a short time (10s)
        -- * if there was network error, e.g. 'MuxBearerClosed' or
        --   'IOException' we don't suspend the peer and allow it to
        --   re-connect immediately.
      , ErrorPolicy
          $ \(e :: MuxError)
                -> case errorType e of
                      MuxUnknownMiniProtocol  -> Just theyBuggy
                      MuxDecodeError          -> Just theyBuggy
                      MuxIngressQueueOverRun  -> Just theyBuggy
                      MuxInitiatorOnly        -> Just theyBuggy
                      -- in case of bearer closed / or IOException we do
                      -- not suspend the peer, it can resume immediately
                      MuxBearerClosed         -> Nothing
                      MuxIOException{}        -> Nothing

        -- Error policy for TxSubmission protocol: outbound side (client role)
      , ErrorPolicy
          $ \(_ :: TxOutbound.TxSubmissionProtocolError)
                -> Just theyBuggy

        -- Error policy for TxSubmission protocol: inbound side (server role)
      , ErrorPolicy
          $ \(_ :: TxInbound.TxSubmissionProtocolError)
                -> Just theyBuggy

        -- Error policy for BlockFetch protocol: consumer side (client role)
      , ErrorPolicy
          $ \(_ :: BlockFetchProtocolFailure)
                -> Just theyBuggy
      ],

      epConErrorPolicies = [
        -- If `connect` thrown an exception, try to re-connect to the peer after
        -- a short delay
        ErrorPolicy $ \(_ :: IOException) -> Just (SuspendConsumer shortDelay)
      ]
    }
  where
    misconfiguredPeer :: SuspendDecision DiffTime
    misconfiguredPeer = SuspendConsumer shortDelay

    theyBuggy :: SuspendDecision DiffTime
    theyBuggy = SuspendPeer shortDelay shortDelay

    shortDelay :: DiffTime
    shortDelay = 10 -- seconds

runByron
  :: Tracer IO (Monitoring.LoggerName, Monitoring.Severity, Text.Builder)
  -> ByronOptions
  -> CSL.Genesis.Config
  -> CSL.BlockConfiguration
  -> CSL.UpdateConfiguration
  -> CSL.NodeConfiguration
  -> Cardano.EpochSlots
  -> SecurityParam
  -> Index IO (Header ByronBlock)
  -> ChainDB IO ByronBlock
  -> Mempool IO ByronBlock TicketNo
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
        genesisBlock =
          CSL.genesisBlock0
            (CSL.consensusEraBVD (CSL.configBlockVersionData genesisConfig))
            (CSL.configProtocolMagic genesisConfig)
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

-- | For use by the Shelley server/client error policy tracers.
defineSeverity :: ErrorPolicyTrace -> Monitoring.Severity
defineSeverity it = case it of
  ErrorPolicySuspendPeer {} -> Monitoring.Warning -- peer misbehaved
  ErrorPolicySuspendConsumer {} -> Monitoring.Notice -- peer temporarily not useful
  ErrorPolicyLocalNodeError {} -> Monitoring.Error
  ErrorPolicyResumePeer {} -> Monitoring.Debug
  ErrorPolicyKeepSuspended {} -> Monitoring.Debug
  ErrorPolicyResumeConsumer {} -> Monitoring.Debug
  ErrorPolicyResumeProducer {} -> Monitoring.Debug
  ErrorPolicyUnhandledApplicationException {} -> Monitoring.Error
  ErrorPolicyUnhandledConnectionException {} -> Monitoring.Error
  ErrorPolicyAcceptException {} -> Monitoring.Error

main :: IO ()
main = withIOManager $ \iocp -> do
  bpo <- Opt.execParser cliParserInfo
  Logging.withLogging (bpoLoggerConfigPath bpo) "cardano_byron_proxy" $ \trace -> do
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
        -- Next, set up the database, taking care to seed with the genesis
        -- block if it's empty.
        dbc :: DBConfig
        dbc = DBConfig
          { dbFilePath    = bpoDatabasePath bpo
          , indexFilePath = bpoIndexPath bpo
          }
    -- Fulfill the necessary cardano-sl reflection configurations.
    CSL.withUpdateConfiguration (CSL.ccUpdate yamlConfig) $
      CSL.withSscConfiguration (CSL.ccSsc yamlConfig) $
      CSL.withBlockConfiguration (CSL.ccBlock yamlConfig) $
      CSL.withDlgConfiguration (CSL.ccDlg yamlConfig) $
      CSL.withNodeConfiguration (CSL.ccNode yamlConfig) $ do
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
            dbTracer :: Tracer IO (TraceEvent ByronBlock)
            dbTracer = Tracer $ \trEvent -> case trEvent of
              TraceAddBlockEvent (AddedBlockToVolDB _point (BlockNo blockno) _) ->
                  -- NB this is here because devops wanted an EKG metric on
                  -- block count. FIXME should be done in a more sane way...
                  let val  = Monitoring.PureI (fromIntegral blockno)
                      item = ("ChainDB", Monitoring.Info, Monitoring.LogValue "blockNum" val)
                  in  doConvertedTrace item
              TraceAddBlockEvent (AddBlockValidation it@(InvalidBlock _ _)) ->
                  let val = ("ChainDB", Monitoring.Error, Monitoring.LogMessage (fromString (show it)))
                  in  doConvertedTrace val
              _ -> pure ()
            indexTracer :: Tracer IO Index.TraceEvent
            indexTracer = Tracer $ \trEvent ->
              let val = ("index", Monitoring.Info, Monitoring.LogMessage (fromString (show trEvent)))
              in  doConvertedTrace val
        traceWith (Logging.convertTrace' trace) ("", Monitoring.Info, fromString "Opening database")
        -- Thread registry is needed by ChainDB and by the network protocols.
        -- I assume it's supposed to be shared?
        ResourceRegistry.withRegistry $ \rr -> do
          let -- The "protocol" version means the "blockchain" version, relevant
              -- to the update system. This is the initial version for updates.
              -- It is _not_ a part of the blockchain genesis, but it
              -- nevertheless must be configured properly or update proposals
              -- may be accepted on one node and not on another. We pull it
              -- from the cardano-sl configuration.
              cslBlockVersion = CSL.lastKnownBlockVersion (CSL.ccUpdate yamlConfig)
              major = CSL.bvMajor cslBlockVersion
              minor = CSL.bvMinor cslBlockVersion
              alt   = CSL.bvAlt   cslBlockVersion
              protocolVersion = Cardano.ProtocolVersion
                { Cardano.pvMajor = major
                , Cardano.pvMinor = minor
                , Cardano.pvAlt   = alt
                }
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
          btime <- realBlockchainTime rr nullTracer systemStart $ focusSlotLengths $ singletonSlotLengths slotDuration
          withDB dbc dbTracer indexTracer rr btime nodeConfig extLedgerState $ \idx cdb -> do
            traceWith (Logging.convertTrace' trace) ("", Monitoring.Info, fromString "Database opened")
            let shelleyClientTracer = contramap (\it -> ("shelley.client", defineSeverity (wiaEvent it), fromString (show it))) (Logging.convertTrace' trace)
                shelleyServerTracer = contramap (\it -> ("shelley.server", defineSeverity (wiaEvent it), fromString (show it))) (Logging.convertTrace' trace)
            Shelley.withShelley rr cdb nodeConfig nodeState btime $ \kernel ctable iversions rversions -> do
              let server = runShelleyServer iocp
                                            shelleyServerTracer
                                            (soLocalAddress (bpoShelleyOptions bpo))
                                            rr
                                            ctable
                                            rversions
                  client = runShelleyClient iocp
                                            shelleyClientTracer
                                            (soProducerAddresses (bpoShelleyOptions bpo))
                                            rr
                                            ctable
                                            iversions
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
                      (configSecurityParam nodeConfig)
                      idx
                      cdb
                      (getMempool kernel)
              _ <- concurrently (concurrently server client) byron
              pure ()
