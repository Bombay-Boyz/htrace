## Unreleased

## 0.1.0.0 - 2026-05-18

### Added
- OTLP/HTTP JSON exporter with configurable endpoint, headers, and timeout
- W3C Trace Context (`traceparent`) propagation — inject and extract
- Async batch processor with configurable queue size, batch size, export
  interval, and overflow strategies (`DropNewest`, `DropOldest`, `BlockProducer`)
- Environment-based configuration via `OTEL_*` and `OTEL_BSP_*` variables
- `alwaysOnSampler`, `alwaysOffSampler`, `traceIdRatioSampler`, `parentBasedSampler`
- `withTracing` bracket for safe tracer lifecycle management
- `inSpan` / `inSpanM` span creation with automatic finalisation
- `setSpanAttr`, `setSpanAttrs`, `setSpanStatus`, `addEvent`, `recordException`
- `memoryExporter` and `noopExporter` for testing
- Internal structured logging via `InternalLogger`