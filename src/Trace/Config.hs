module Trace.Config
  ( -- * Top-level config
    TracingConfig (..)
  , defaultConfig
    -- * Resource
  , Resource
  , unResource
  , mkResource
  , defaultResource
    -- * Propagator
  , Propagator (..)
    -- * Exporter config
  , ExporterConfig (..)
    -- * Sampler config
  , SamplerConfig (..)
  , SampleRate
  , mkSampleRate
  , unSampleRate
    -- * Errors
  , ConfigError (..)
  , EnvVarName (..)
    -- * Environment loader
  , fromEnv
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read qualified as TR
import System.Environment (lookupEnv)

import Trace.Attributes
import Trace.Core
import Trace.Export.Otlp
import Trace.Export.Types

-- ---------------------------------------------------------------------------
-- Error types
-- ---------------------------------------------------------------------------

-- | The name of an environment variable, for use in error messages.
newtype EnvVarName = EnvVarName { unEnvVarName :: Text }
  deriving stock (Show, Eq)

-- | All errors that can arise when loading 'TracingConfig'.
data ConfigError
  = MissingRequiredVar       !EnvVarName
  | InvalidVarValue          !EnvVarName !Text !Text
  | InvalidEndpointUrl       !Text
  | InvalidSampleRate        !Double
  | InvalidExporterInit      !ExporterInitError
  | UnsupportedOtlpProtocol  !Text
  | InvalidResourceAttribute !Text !Text
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Validation applicative
-- ---------------------------------------------------------------------------

-- | A simple validation type that accumulates errors rather than
-- short-circuiting. We implement it here rather than importing
-- the `validation` package to keep the dependency surface small.
data Validation e a
  = Failure e
  | Success a

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Semigroup e => Applicative (Validation e) where
  pure = Success
  Failure e1 <*> Failure e2 = Failure (e1 <> e2)
  Failure e  <*> _          = Failure e
  _          <*> Failure e  = Failure e
  Success f  <*> Success a  = Success (f a)

validationToEither :: Validation e a -> Either e a
validationToEither (Failure e) = Left e
validationToEither (Success a) = Right a

traverseValidation
  :: Semigroup e
  => (a -> Validation e b)
  -> [a]
  -> Validation e [b]
traverseValidation _ []     = Success []
traverseValidation f (x:xs) =
  (:) <$> f x <*> traverseValidation f xs

-- ---------------------------------------------------------------------------
-- Propagator
-- ---------------------------------------------------------------------------

data Propagator = W3CTraceContextPropagator
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Resource
-- ---------------------------------------------------------------------------

-- | A set of attributes describing the entity producing spans.
newtype Resource = Resource { unResource :: SpanAttrs }
  deriving stock (Show, Eq)

-- | Build a 'Resource' with a non-empty service name and optional extra attrs.
mkResource
  :: Text
  -> [(AttrKey, AttrValue)]
  -> Either ConfigError Resource
mkResource serviceName extra
  | Text.null (Text.strip serviceName) =
      Left (InvalidVarValue
              (EnvVarName "OTEL_SERVICE_NAME")
              serviceName
              "service.name must be non-empty")
  | otherwise =
      Right $ Resource $ attrs $
        (AttrKey "service.name", AttrString serviceName) : extra

-- | Default resource with SDK identification attributes.
defaultResource :: Resource
defaultResource = Resource $ attrs
  [ (AttrKey "service.name",           AttrString "htrace-default")
  , (AttrKey "telemetry.sdk.name",     AttrString "htrace")
  , (AttrKey "telemetry.sdk.version",  AttrString "0.1.0.0")
  , (AttrKey "telemetry.sdk.language", AttrString "haskell")
  ]

-- ---------------------------------------------------------------------------
-- Exporter config
-- ---------------------------------------------------------------------------

data ExporterConfig
  = OtlpExporter !OtlpConfig
  | NoopExporter
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Sampler config
-- ---------------------------------------------------------------------------

-- | A sampling rate in [0, 1].
newtype SampleRate = SampleRate { unSampleRate :: Double }
  deriving stock (Show, Eq, Ord)

-- | Returns 'Left' if the value is outside [0, 1].
mkSampleRate :: Double -> Either ConfigError SampleRate
mkSampleRate d
  | d >= 0 && d <= 1 = Right (SampleRate d)
  | otherwise        = Left (InvalidSampleRate d)

data SamplerConfig
  = AlwaysSample
  | NeverSample
  | TraceIdRatio !SampleRate
  | ParentBased  !SamplerConfig
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- TracingConfig
-- ---------------------------------------------------------------------------

data TracingConfig = TracingConfig
  { configExporter    :: !ExporterConfig
  , configSampler     :: !SamplerConfig
  , configResource    :: !Resource
  , configPropagators :: ![Propagator]
  , configLogger      :: !InternalLogger
  }

instance Show TracingConfig where
  show c =
    "TracingConfig { exporter = "    <> show (configExporter c)
    <> ", sampler = "                <> show (configSampler c)
    <> ", resource = "               <> show (configResource c)
    <> ", propagators = "            <> show (configPropagators c)
    <> ", logger = <function> }"

-- | Two 'TracingConfig' values are equal when all fields except
-- 'configLogger' are equal (functions cannot be compared).
instance Eq TracingConfig where
  a == b =
    configExporter    a == configExporter    b
    && configSampler     a == configSampler     b
    && configResource    a == configResource    b
    && configPropagators a == configPropagators b

-- | Safe defaults: noop exporter, always sample, default resource.
defaultConfig :: TracingConfig
defaultConfig = TracingConfig
  { configExporter    = NoopExporter
  , configSampler     = AlwaysSample
  , configResource    = defaultResource
  , configPropagators = [W3CTraceContextPropagator]
  , configLogger      = stderrLogger
  }

-- ---------------------------------------------------------------------------
-- Environment loader
-- ---------------------------------------------------------------------------

-- | Load 'TracingConfig' from @OTEL_*@ environment variables.
-- Accumulates all errors rather than short-circuiting on the first one.
-- Honours @OTEL_SDK_DISABLED=true@ as a kill-switch.
fromEnv :: IO (Either (NonEmpty ConfigError) TracingConfig)
fromEnv = do
  disabled <- lookupEnv "OTEL_SDK_DISABLED"
  case disabled of
    Just s | Text.toLower (Text.pack s) == "true" ->
      pure (Right (defaultConfig { configExporter = NoopExporter }))
    _ -> do
      vExp   <- loadExporterConfig
      vSamp  <- loadSamplerConfig
      vRes   <- loadResource
      pure $ validationToEither $
        TracingConfig
          <$> vExp
          <*> vSamp
          <*> vRes
          <*> pure [W3CTraceContextPropagator]
          <*> pure stderrLogger

-- ---------------------------------------------------------------------------
-- Exporter loader
-- ---------------------------------------------------------------------------

loadExporterConfig
  :: IO (Validation (NonEmpty ConfigError) ExporterConfig)
loadExporterConfig = do
  kind <- lookupEnv "OTEL_TRACES_EXPORTER"
  case kind of
    Just "none" -> pure (Success NoopExporter)
    Just "otlp" -> loadOtlpConfig
    Nothing     -> loadOtlpConfig
    Just other  -> pure $ Failure $ NE.singleton $
      InvalidVarValue
        (EnvVarName "OTEL_TRACES_EXPORTER")
        (Text.pack other)
        "expected 'otlp' or 'none'"

loadOtlpConfig
  :: IO (Validation (NonEmpty ConfigError) ExporterConfig)
loadOtlpConfig = do
  endpointVar <- lookupEnv "OTEL_EXPORTER_OTLP_ENDPOINT"
  protoVar    <- lookupEnv "OTEL_EXPORTER_OTLP_PROTOCOL"
  hdrsVar     <- lookupEnv "OTEL_EXPORTER_OTLP_HEADERS"
  timeoutVar  <- lookupEnv "OTEL_EXPORTER_OTLP_TIMEOUT"
  let vEndpoint = case endpointVar of
        Nothing -> Failure $ NE.singleton $
          MissingRequiredVar (EnvVarName "OTEL_EXPORTER_OTLP_ENDPOINT")
        Just s  -> case mkEndpoint (Text.pack s) of
          Left (ExporterInvalidEndpoint t)   ->
            Failure $ NE.singleton (InvalidEndpointUrl t)
          Left (ExporterUnsupportedScheme t) ->
            Failure $ NE.singleton (UnsupportedOtlpProtocol t)
          Left other ->
            Failure $ NE.singleton (InvalidExporterInit other)
          Right ep -> Success ep

      vProto = case protoVar of
        Nothing          -> Success ()
        Just "http/json" -> Success ()
        Just other       -> Failure $ NE.singleton $
          UnsupportedOtlpProtocol (Text.pack other)

      vHeaders = case hdrsVar of
        Nothing -> Success []
        Just s  -> parseHeaderList (Text.pack s)

      vTimeout = case timeoutVar of
        Nothing -> Success 10
        Just s  -> case TR.double (Text.pack s) of
          Right (d, rest) | Text.null rest && d > 0 ->
            Success (realToFrac d)
          _ -> Failure $ NE.singleton $
            InvalidVarValue
              (EnvVarName "OTEL_EXPORTER_OTLP_TIMEOUT")
              (Text.pack s)
              "expected a positive number of seconds"

      assemble ep hdrs to =
        OtlpExporter (OtlpConfig ep hdrs to NoCompression)

  -- vProto contributes only errors; use (<* vProto) to require it to
  -- succeed without carrying its value forward.
  pure $ (assemble <$> vEndpoint <*> vHeaders <*> vTimeout) <* vProto

-- | Parse @"k1=v1,k2=v2"@ into a list of header pairs.
parseHeaderList
  :: Text
  -> Validation (NonEmpty ConfigError) [(Text, Text)]
parseHeaderList t =
  traverseValidation parseOne
    (filter (not . Text.null) (map Text.strip (Text.splitOn "," t)))
  where
    parseOne entry = case Text.splitOn "=" entry of
      (k:rest) | not (Text.null (Text.strip k)) ->
        Success (Text.strip k, Text.strip (Text.intercalate "=" rest))
      _ -> Failure $ NE.singleton $
        InvalidVarValue
          (EnvVarName "OTEL_EXPORTER_OTLP_HEADERS")
          entry
          "expected key=value"

-- ---------------------------------------------------------------------------
-- Sampler loader
-- ---------------------------------------------------------------------------

loadSamplerConfig
  :: IO (Validation (NonEmpty ConfigError) SamplerConfig)
loadSamplerConfig = do
  s <- lookupEnv "OTEL_TRACES_SAMPLER"
  case s of
    Nothing           -> pure (Success AlwaysSample)
    Just "always_on"  -> pure (Success AlwaysSample)
    Just "always_off" -> pure (Success NeverSample)
    Just "traceidratio" -> do
      arg <- lookupEnv "OTEL_TRACES_SAMPLER_ARG"
      case arg of
        Nothing -> pure $ Failure $ NE.singleton $
          MissingRequiredVar (EnvVarName "OTEL_TRACES_SAMPLER_ARG")
        Just s  -> case parseDouble s of
          Nothing -> pure $ Failure $ NE.singleton $
            InvalidVarValue
              (EnvVarName "OTEL_TRACES_SAMPLER_ARG")
              (Text.pack s)
              "expected a number in [0, 1]"
          Just d  -> case mkSampleRate d of
            Left e   -> pure $ Failure $ NE.singleton e
            Right sr -> pure $ Success (TraceIdRatio sr)
    Just "parentbased_always_on"  ->
      pure (Success (ParentBased AlwaysSample))
    Just "parentbased_always_off" ->
      pure (Success (ParentBased NeverSample))
    Just "parentbased_traceidratio" -> do
      arg <- lookupEnv "OTEL_TRACES_SAMPLER_ARG"
      case arg of
        Nothing -> pure $ Failure $ NE.singleton $
          MissingRequiredVar (EnvVarName "OTEL_TRACES_SAMPLER_ARG")
        Just s  -> case parseDouble s of
          Nothing -> pure $ Failure $ NE.singleton $
            InvalidVarValue
              (EnvVarName "OTEL_TRACES_SAMPLER_ARG")
              (Text.pack s)
              "expected a number in [0, 1]"
          Just d  -> case mkSampleRate d of
            Left e   -> pure $ Failure $ NE.singleton e
            Right sr -> pure $ Success (ParentBased (TraceIdRatio sr))
    Just other -> pure $ Failure $ NE.singleton $
      InvalidVarValue
        (EnvVarName "OTEL_TRACES_SAMPLER")
        (Text.pack other)
        "expected 'always_on', 'always_off', 'traceidratio', or 'parentbased_*'"
  where
    parseDouble str = case TR.double (Text.pack str) of
      Right (d, rest) | Text.null rest -> Just d
      _                                -> Nothing

-- ---------------------------------------------------------------------------
-- Resource loader
-- ---------------------------------------------------------------------------

loadResource
  :: IO (Validation (NonEmpty ConfigError) Resource)
loadResource = do
  svc <- lookupEnv "OTEL_SERVICE_NAME"
  raw <- lookupEnv "OTEL_RESOURCE_ATTRIBUTES"
  let vExtraKvs = case raw of
        Nothing -> Success []
        Just "" -> Success []
        Just t  ->
          traverseValidation parseOne
            (filter (not . Text.null)
               (map Text.strip (Text.splitOn "," (Text.pack t))))
  case svc of
    Nothing -> pure (Success defaultResource)
    Just "" -> pure $ Failure $ NE.singleton $
      InvalidVarValue
        (EnvVarName "OTEL_SERVICE_NAME")
        ""
        "service.name must be non-empty"
    Just s  ->
      pure $ case vExtraKvs of
        Failure errs -> Failure errs
        Success kvs  -> case mkResource (Text.pack s) kvs of
          Left e  -> Failure (NE.singleton e)
          Right r -> Success r
  where
    parseOne
      :: Text
      -> Validation (NonEmpty ConfigError) (AttrKey, AttrValue)
    parseOne entry = case Text.splitOn "=" entry of
      (k:rest) | not (Text.null (Text.strip k)) && not (null rest) ->
        Success
          ( AttrKey (Text.strip k)
          , AttrString (Text.strip (Text.intercalate "=" rest))
          )
      _ -> Failure $ NE.singleton $
        InvalidResourceAttribute entry "expected key=value"