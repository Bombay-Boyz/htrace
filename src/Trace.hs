-- | The @htrace@ public API.
--
-- Import this module for everything you need to instrument your application
-- with OpenTelemetry-compliant distributed tracing.
module Trace
  ( -- * Entry point
    withTracing

    -- * Tracer
  , Tracer
  , tracerScope

    -- * Span creation
  , inSpan
  , inSpanM
  , Span
  , SpanName
  , mkSpanName
  , SpanKind (..)

    -- * Span mutators
  , setSpanAttr
  , setSpanAttrs
  , setSpanStatus
  , setStatusError
  , recordException
  , addEvent
  , SpanError (..)

    -- * Attributes
  , attrs
  , lookupAttr
  , AttrKey (..)
  , AttrValue (..)
  , SpanAttrs
  , MissingAttr (..)

    -- * Span status
  , SpanStatus (..)
  , ErrorMessage
  , mkErrorMessage
  , unErrorMessage

    -- * Tracing context
  , TraceContext (..)
  , TraceM
  , getCurrentSpanContext
  , SpanContext (..)
  , TraceId
  , unTraceId
  , SpanId
  , unSpanId
  , TraceFlags
  , defaultTraceFlags
  , isSampled
  , setSampled

    -- * Propagation
  , parseTraceparent
  , emitTraceparent
  , injectHeaders
  , extractContext
  , PropagationResult (..)
  , PropagationError (..)

    -- * Configuration
  , TracingConfig (..)
  , defaultConfig
  , fromEnv
  , ConfigError (..)
  , EnvVarName (..)
  , Resource
  , mkResource
  , unResource
  , SamplerConfig (..)
  , SampleRate
  , mkSampleRate
  , unSampleRate
  , ExporterConfig (..)
  , Propagator (..)

    -- * Operational
  , flush
  , InternalLogger (..)
  , stderrLogger

    -- * Errors
  , ExporterInitError (..)
  , ExportResult (..)
  , ExportError (..)
  ) where

import Trace.Attributes
import Trace.Config
import Trace.Core
import Trace.Export.Types
import Trace.Monad
import Trace.Propagation