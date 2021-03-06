{-# LANGUAGE OverloadedStrings #-}

module Logging where

import Control.Exception (bracket)
import Control.Tracer (Tracer (..))
import Data.Functor.Contravariant (contramap)
import Data.Text (Text)
import qualified Data.Text.Lazy as Text (toStrict)
import qualified Data.Text.Lazy.Builder as Text

import qualified Cardano.BM.Configuration.Model as Monitoring (setupFromRepresentation)
import qualified Cardano.BM.Backend.EKGView
import qualified Cardano.BM.Data.BackendKind as Monitoring
import qualified Cardano.BM.Data.Configuration as Monitoring
import qualified Cardano.BM.Data.LogItem as Monitoring
import qualified Cardano.BM.Data.Output as Monitoring
import qualified Cardano.BM.Data.Severity as Monitoring
import           Cardano.BM.Plugin (loadPlugin)
import qualified Cardano.BM.Setup as Monitoring (setupTrace_, shutdown)
import qualified Cardano.BM.Trace as Monitoring (Trace)

-- | Set up logging using an optional configuration file.
withLogging
  :: Maybe FilePath -- ^ Config file
  -> Text           -- ^ Logger name
  -> (Monitoring.Trace IO Text -> IO t)
  -> IO t
withLogging mLoggerConfig name k = do
  -- Set up logging. If there's a config file we read it, otherwise use a
  -- default.
  loggerConfig <- case mLoggerConfig of
    Nothing -> pure defaultLoggerConfig
    Just fp -> Monitoring.readRepresentation fp
  -- iohk-monitoring uses some MVar for configuration, which corresponds to
  -- the "Representation" which we call config.
  loggerConfig' <- Monitoring.setupFromRepresentation loggerConfig
  bracket
    (do (tr0, sb0) <- Monitoring.setupTrace_ loggerConfig' name
        -- load plugin for EKG and Prometheus backend
        Cardano.BM.Backend.EKGView.plugin loggerConfig' tr0 sb0
          >>= loadPlugin sb0
        return (tr0, sb0)
    )
    (\(_,sb) -> Monitoring.shutdown sb)
    (\(tr,_) -> k tr)

-- | `withTrace` from the monitoring framework gives us a trace that
-- works on `LogObjects`. `convertTrace` will make it into a `Trace IO Text`
-- which fills in the `LogObject` details.
--
-- It's not a contramap, because it requires grabbing the thread id and
-- the current time.
convertTrace
  :: Monitoring.Trace IO a
  -> Tracer IO (Monitoring.LoggerName, Monitoring.Severity, Monitoring.LOContent a)
convertTrace trace = case trace of
  Tracer f -> Tracer $ \(name, sev, content) -> do
    logMeta <- Monitoring.mkLOMeta sev Monitoring.Public
    let logContent = content
        logObject  = Monitoring.LogObject [name] logMeta logContent
    f logObject

convertTrace'
  :: Monitoring.Trace IO Text
  -> Tracer IO (Monitoring.LoggerName, Monitoring.Severity, Text.Builder)
convertTrace' = contramap f . convertTrace
  where
  f (a, b, builder) = (a, b, Monitoring.LogMessage (Text.toStrict (Text.toLazyText builder)))

-- | It's called `Representation` but is closely related to the `Configuration`
-- from iohk-monitoring. The latter has to do with `MVar`s. It's all very
-- weird.
defaultLoggerConfig :: Monitoring.Representation
defaultLoggerConfig = Monitoring.Representation
  { Monitoring.minSeverity     = Monitoring.Debug
  , Monitoring.rotation        = Nothing
  , Monitoring.setupScribes    = [stdoutScribe]
  , Monitoring.defaultScribes  = [(Monitoring.StdoutSK, "stdout")]
  , Monitoring.setupBackends   = [Monitoring.KatipBK]
  , Monitoring.defaultBackends = [Monitoring.KatipBK]
  , Monitoring.hasEKG          = Nothing
  , Monitoring.hasPrometheus   = Nothing
  , Monitoring.hasGUI          = Nothing
  , Monitoring.options         = mempty
  , Monitoring.hasGraylog      = Nothing
  , Monitoring.logOutput       = Nothing
  }
  where
  stdoutScribe = Monitoring.ScribeDefinition
    { Monitoring.scKind     = Monitoring.StdoutSK
    , Monitoring.scFormat   = Monitoring.ScText
    , Monitoring.scName     = "stdout"
    , Monitoring.scPrivacy  = Monitoring.ScPublic
    , Monitoring.scRotation = Nothing
    }
