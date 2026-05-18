# htrace

A lightweight, idiomatic Haskell distributed tracing SDK focused on
correctness, composability, and OpenTelemetry compatibility.

## Features

- OTLP/HTTP JSON export (W3C Trace Context compatible)
- W3C `traceparent` header propagation
- Async batching with configurable overflow strategies
- Environment-based configuration via standard `OTEL_*` variables
- Purely functional sampler composition

## Quick start

```haskell
import Trace.Config (fromEnvWithStderr)
import Trace.Monad  (withTracing, inSpan)
import Trace.Core   (Internal)
import Trace.Attributes (mempty)

main :: IO ()
main = do
  cfg <- fromEnvWithStderr >>= either (fail . show) pure
  withTracing cfg $ \tracer ->
    inSpan tracer "my-operation" Internal mempty $ \_span ->
      putStrLn "Hello from a traced span"
```

## Environment variables

| Variable | Default | Description |
|---|---|---|
| `OTEL_TRACES_EXPORTER` | `otlp` | `otlp` or `none` |
| `OTEL_EXPORTER_OTLP_ENDPOINT` | *(required for otlp)* | e.g. `http://localhost:4318` |
| `OTEL_EXPORTER_OTLP_HEADERS` | *(none)* | `k1=v1,k2=v2` |
| `OTEL_EXPORTER_OTLP_TIMEOUT` | `10` | Timeout in seconds |
| `OTEL_TRACES_SAMPLER` | `always_on` | `always_on`, `always_off`, `traceidratio`, `parentbased_*` |
| `OTEL_TRACES_SAMPLER_ARG` | *(none)* | Required for `traceidratio` — a float in `[0, 1]` |
| `OTEL_SERVICE_NAME` | `htrace-default` | Service name in resource attributes |
| `OTEL_SDK_DISABLED` | `false` | Set to `true` to disable all tracing |
| `OTEL_BSP_MAX_QUEUE_SIZE` | `2048` | Batch processor queue depth |
| `OTEL_BSP_MAX_EXPORT_BATCH_SIZE` | `512` | Max spans per export call |
| `OTEL_BSP_SCHEDULE_DELAY` | `5000` | Export interval in milliseconds |
| `OTEL_BSP_EXPORT_TIMEOUT` | `30000` | Per-export timeout in milliseconds |
| `OTEL_BSP_SHUTDOWN_TIMEOUT` | `5000` | Graceful shutdown deadline in milliseconds |

## Compatibility

- GHC 9.4 or later (GHC2021 language edition)
- Stackage LTS 22.x / Nightly
- OTLP protocol: `http/json` only (gzip compression planned)