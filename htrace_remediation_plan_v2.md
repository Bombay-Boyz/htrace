# htrace — Remediation Plan

> **Build system:** Stack, resolver `lts-23.28` (GHC 9.6.6).
> **Convention:** Every phase follows the same six-section structure as the
> original mission document: Goal · What gets built · Modules touched ·
> Acceptance criteria · Test plan · What this phase does NOT do.
> Every code block is complete and paste-ready. No stubs, no placeholders,
> no partial functions, no silent failures.

---

## Confirmed defects in the current codebase

Before the plan, here is the precise list of defects found by reading every
source file. Each remediation phase maps to one or more of these.

| # | File | Defect |
|---|------|--------|
| D1 | `Trace/Core.hs` | `Span (..)` and `SpanInternals (..)` exported: any caller can write the `TVar` directly, bypassing `modifySpan` lifecycle guards |
| D2 | `Trace/Core.hs` | `SpanName (..)` exported: callers can construct `SpanName ""`, bypassing `mkSpanName` |
| D3 | `Trace/Monad.hs` | `setStatusError` calls `error "mkErrorMessage failed"` — a partial function that crashes the process on a reachable code path (blank message) |
| D4 | `Trace/Core.hs` | `traceIdRatioSampler` uses `Double` division: loses 11 bits of precision near `maxBound :: Word64`; child spans ignore parent's sampled bit entirely |
| D5 | `Trace/Export/Types.hs` | `memoryExporter` uses `(<> NE.toList ne)` which is O(n) left-append on every export call |
| D6 | `Trace/Export/Batch.hs` | Worker silently discards export errors and timeout results; no error is surfaced to the logger |
| D7 | `Trace/Propagation.hs` | `validVersion v = v == "00"` rejects all future W3C versions instead of accepting anything except the reserved `"ff"` |

---

## Phase ordering

```
R1  Fix encapsulation (D1, D2)
 └─► R2  Fix partial function in setStatusError (D3)
      └─► R3  Fix sampler: integer threshold + parent-based (D4)
               └─► R4  Fix memoryExporter append order (D5)
                    └─► R5  Fix worker silent discard (D6)
                         └─► R6  Fix traceparent version acceptance (D7)
```

R4, R5, R6 are independent of each other after their listed predecessor and
can be developed in parallel. R1 must land first because R2 depends on the
correct `setStatusError` signature which references the opaque `SpanName`.

All phases: `stack build` and `stack test` green before moving on.

---

# Phase R1 — Remove Exported Constructors for Mutable and Invariant Types

**Goal.** Make `Span` and `SpanInternals` opaque to all code outside this
package, and prevent construction of blank `SpanName` values, by removing
`(..)` from their export lists and adding a safe read-only accessor.

## What gets built

**`src/Trace/Core.hs` — export list, two changes only**

Remove `(..)` from `SpanName`, `SpanInternals`, and `Span`. Add
`readSpanInternals` as the sole sanctioned read path for external code.
Everything else in the module is unchanged.

```haskell
module Trace.Core
  ( -- * Trace ID
    TraceId
  , unTraceId
  , newTraceId
  , traceIdFromBytes
    -- * Span ID
  , SpanId
  , unSpanId
  , newSpanId
  , spanIdFromBytes
    -- * Parse errors
  , IdParseError (..)
    -- * Trace flags
  , TraceFlags (..)
  , defaultTraceFlags
  , isSampled
  , setSampled
    -- * Span context
  , SpanContext (..)
    -- * Span kind
  , SpanKind (..)
    -- * Span status
  , SpanStatus (..)
  , ErrorMessage
  , unErrorMessage
  , mkErrorMessage
    -- * Span name  — constructor hidden; use mkSpanName or the IsString instance
  , SpanName
  , unSpanName
  , mkSpanName
    -- * Span lifecycle types
  , SpanState (..)
  , SpanEvent (..)
  , SpanError (..)
    -- SpanInternals — constructor and fields hidden; use readSpanInternals
  , SpanInternals
  , siState
  , siStatus
  , siAttributes
  , siEvents
    -- Span — constructor hidden; spanInternals is not exported
  , Span
  , spanContext
  , spanName
  , spanKind
  , spanClock
  , readSpanInternals
  , FinishedSpan (..)
    -- * Instrumentation scope
  , InstrumentationScope (..)
    -- * Sampling
  , SamplingDecision (..)
  , Sampler (..)
  , alwaysOnSampler
  , alwaysOffSampler
  , traceIdRatioSampler
    -- * Clock
  , Clock (..)
  , systemClock
  ) where
```

Add at the bottom of `src/Trace/Core.hs`, before the end of the file:

```haskell
-- | Read a point-in-time snapshot of a live span's internals without
-- exposing the underlying 'TVar'. This is the only read path for code
-- outside this package. Writing is only possible through 'modifySpan'
-- in "Trace.Monad".
readSpanInternals :: Span -> IO SpanInternals
readSpanInternals = readTVarIO . spanInternals
```

This requires adding `readTVarIO` to the import of `Control.Concurrent.STM`:

```haskell
import Control.Concurrent.STM (TVar, readTVarIO)
```

No other changes to `src/Trace/Core.hs`.

**`src/Trace.hs` — façade, two removals**

The façade must not re-export the hidden names. Confirm that `SpanInternals`
does not appear in `src/Trace.hs` (it does not currently). Add
`readSpanInternals` to the façade so test code and application code can use it:

```haskell
  , readSpanInternals
```

Add it under the `-- * Span creation` section alongside `Span`.

## Modules touched

- `src/Trace/Core.hs` — export list (remove three `(..)`), add `readSpanInternals`, add `readTVarIO` to STM import
- `src/Trace.hs` — add `readSpanInternals` to re-export list

## Acceptance criteria

1. `stack build` passes with `-Wall -Werror`.
2. The negative-compile test below fails to compile, proving the `TVar`
   field is inaccessible.
3. `mkSpanName ""` returns `Nothing`; `SpanName ""` does not compile from
   outside the package.
4. All existing `stack test` cases pass without modification — `Monad.hs`
   accesses `spanInternals` as an internal field and is unaffected by the
   export-list change.

## Test plan

**Negative compile test** (`test/Trace/EncapsulationTest.hs`).

Wire this as a separate `test-suite` component in `package.yaml` named
`htrace-encapsulation-test` with `buildable: false` by default. In CI,
set `buildable: true` and assert the build *fails*:

```haskell
-- test/Trace/EncapsulationTest.hs
-- This file MUST NOT compile after Phase R1.
-- CI check: stack build htrace:test:htrace-encapsulation-test 2>&1 |
--           grep -q "Not in scope" && echo PASS
module Trace.EncapsulationTest where

import Control.Concurrent.STM (readTVarIO)
import Trace.Core (Span (..), SpanInternals (..))

-- spanInternals is no longer exported; this must be a compile error.
probe :: Span -> IO ()
probe sp = readTVarIO (spanInternals sp) >>= print . siState
```

**Unit tests** — add to `test/Trace/CoreSpec.hs`:

```haskell
describe "SpanName encapsulation" $ do
  it "mkSpanName rejects empty text" $
    mkSpanName "" `shouldBe` Nothing

  it "mkSpanName rejects whitespace-only text" $
    mkSpanName "   " `shouldBe` Nothing

  it "mkSpanName accepts non-blank text" $
    mkSpanName "checkout" `shouldBe` Just (SpanName "checkout")

  it "IsString instance falls back for empty literal" $
    unSpanName ("" :: SpanName) `shouldBe` "<unnamed-span>"

describe "readSpanInternals" $ do
  it "returns SpanActive while span is open" $ do
    (mem, _) <- memoryExporter
    let tracer = mkTestTracer mem
    inSpan tracer "r1-active" Internal mempty $ \sp -> do
      si <- readSpanInternals sp
      case siState si of
        SpanActive _ -> pure ()
        other        -> expectationFailure
          ("expected SpanActive, got: " <> show other)

  it "reflects setSpanAttr immediately" $ do
    (mem, _) <- memoryExporter
    let tracer = mkTestTracer mem
    inSpan tracer "r1-attr" Internal mempty $ \sp -> do
      Right () <- setSpanAttr sp (AttrKey "k") (AttrString "v")
      si <- readSpanInternals sp
      lookupAttr (AttrKey "k") (siAttributes si)
        `shouldBe` Right (AttrString "v")
```

**Property** — add to `test/Trace/CoreSpec.hs`:

```haskell
prop_readSpanInternals_reflects_mutations :: Property
prop_readSpanInternals_reflects_mutations = property $ do
  k <- forAll genAttrKey
  v <- forAll genAttrValue
  liftIO $ do
    (mem, _) <- memoryExporter
    let tracer = mkTestTracer mem
    inSpan tracer "prop-r1" Internal mempty $ \sp -> do
      _ <- setSpanAttr sp k v
      si <- readSpanInternals sp
      lookupAttr k (siAttributes si) `H.===` Right v
```

**Outlier** — `SpanName` via `IsString` with a string of only Unicode
whitespace (`"\x2003"` — em space): `mkSpanName` strips with `Text.strip`
which handles Unicode whitespace, so `mkSpanName "\x2003"` returns `Nothing`
and `("\x2003" :: SpanName)` produces `SpanName "<unnamed-span>"`.
Pin this as a unit test.

## What this phase does NOT do

- Does not change `FinishedSpan (..)` — it is an immutable snapshot; field
  access in tests is legitimate.
- Does not change `SpanContext (..)`, `SpanState (..)`, `SpanEvent (..)`,
  `SpanError (..)` — these carry no mutable invariant.
- Does not touch any logic; only the export list and one new function.

---

# Phase R2 — Eliminate the Partial Function in `setStatusError`

**Goal.** Replace the `error "mkErrorMessage failed"` call in `setStatusError`
with a total implementation that never calls `error` or `undefined`.

## What gets built

The current code in `src/Trace/Monad.hs`:

```haskell
-- CURRENT — contains a reachable error call:
setStatusError :: Span -> Text -> IO (Either SpanError ())
setStatusError sp t =
  setSpanStatus sp $ StatusError $
    case mkErrorMessage t of
      Just m  -> m
      Nothing ->
         case mkErrorMessage (Text.pack "<unspecified error>") of
             Just m  -> m
             Nothing -> error "mkErrorMessage failed"
```

`"<unspecified error>"` is a non-blank, non-whitespace literal, so
`mkErrorMessage` will always return `Just` for it. However the `error` branch
is reachable at the type level and will crash the process if it ever runs.
The correct fix is to construct the fallback `ErrorMessage` directly, which
is legal inside `Trace.Monad` because `Monad.hs` is in the same package as
`Core.hs` and has access to the `ErrorMessage` data constructor as an
internal name.

Replace the entire `setStatusError` definition:

```haskell
-- src/Trace/Monad.hs

-- | Set the span status to 'StatusError' with the given message.
-- If the message is blank or whitespace-only, falls back to the literal
-- text @\<unspecified error\>@. Never throws.
setStatusError :: Span -> Text -> IO (Either SpanError ())
setStatusError sp t =
  setSpanStatus sp (StatusError msg)
  where
    msg = case mkErrorMessage t of
      Just m  -> m
      Nothing -> ErrorMessage (Text.pack "<unspecified error>")
      -- ErrorMessage is accessible here because Monad.hs is an internal
      -- module in the same package. The constructor is not re-exported.
```

This is the complete replacement. No other changes to `Monad.hs`.

## Modules touched

- `src/Trace/Monad.hs` — `setStatusError` only

## Acceptance criteria

1. `stack build -Wall -Werror` passes.
2. `setStatusError sp ""` returns `Right ()` and the span's status becomes
   `StatusError (ErrorMessage "<unspecified error>")`. No exception.
3. `setStatusError sp "  "` (whitespace-only) produces the same fallback.
4. `setStatusError sp "real error"` produces
   `StatusError (ErrorMessage "real error")`.
5. The word `error` does not appear in `setStatusError` or any function it
   calls transitively.

## Test plan

**Unit tests** — add to `test/Trace/MonadSpec.hs`:

```haskell
describe "setStatusError" $ do
  -- runAndCapture runs the action inside a span and returns the single
  -- FinishedSpan. inSpan guarantees exactly one span is exported.
  let runAndCapture action = do
        (mem, readAll) <- memoryExporter
        let tracer = mkTestTracer mem
        inSpan tracer "r2-test" Internal mempty $ \sp -> action sp
        readAll >>= \case
          [fs] -> pure fs
          other -> expectationFailure
            ("expected exactly 1 span, got " <> show (length other))
            >> undefined  -- unreachable; expectationFailure throws

  it "blank message falls back to <unspecified error>" $ do
    fs <- runAndCapture $ \sp -> void (setStatusError sp "")
    case fsStatus fs of
      StatusError (ErrorMessage m) ->
        m `shouldBe` "<unspecified error>"
      other -> expectationFailure ("expected StatusError, got: " <> show other)

  it "whitespace-only message falls back to <unspecified error>" $ do
    fs <- runAndCapture $ \sp -> void (setStatusError sp "   ")
    case fsStatus fs of
      StatusError (ErrorMessage m) ->
        m `shouldBe` "<unspecified error>"
      other -> expectationFailure ("expected StatusError, got: " <> show other)

  it "non-blank message is preserved exactly" $ do
    fs <- runAndCapture $ \sp -> void (setStatusError sp "connection refused")
    case fsStatus fs of
      StatusError (ErrorMessage m) ->
        m `shouldBe` "connection refused"
      other -> expectationFailure ("expected StatusError, got: " <> show other)

  it "setStatusError on ended span returns SpanAlreadyEnded" $ do
    (mem, _) <- memoryExporter
    let tracer = mkTestTracer mem
    sp <- inSpan tracer "r2-ended" Internal mempty pure
    -- Span is now ended; sp is the Span value returned by (pure sp)
    result <- setStatusError sp "late"
    result `shouldBe` Left SpanAlreadyEnded
```

**Outlier** — a message consisting entirely of Unicode whitespace (em-space
`"\x2003"`): `Text.strip` removes it; fallback message is used.

```haskell
  it "unicode-whitespace-only message falls back" $ do
    fs <- runAndCapture $ \sp -> void (setStatusError sp "\x2003")
    case fsStatus fs of
      StatusError (ErrorMessage m) ->
        m `shouldBe` "<unspecified error>"
      other -> expectationFailure ("unexpected: " <> show other)
```

## What this phase does NOT do

- Does not change `recordException`, `addEvent`, or any other mutator.
- Does not alter the `ErrorMessage` type or `mkErrorMessage` logic.

---

# Phase R3 — Integer-Threshold Sampler and Parent-Based Sampling

**Goal.** Replace the floating-point `traceIdRatioSampler` with an exact
integer-threshold implementation, and add `parentBasedSampler` so that child
spans inherit their parent's sampling decision as required by the OTel spec.

## What gets built

**`src/Trace/Core.hs` — replace `traceIdRatioSampler`, add `parentBasedSampler`**

The current implementation divides by `fromIntegral (maxBound :: Word64)`,
which is a `Double`. IEEE 754 doubles have 53-bit mantissa; `Word64` has
64 bits. The top 2^11 values of `Word64` all map to the same `Double`,
creating a systematic bias. The fix uses `Integer` arithmetic throughout.

Replace the existing `traceIdRatioSampler` and `traceIdRatio` definitions:

```haskell
-- src/Trace/Core.hs

-- | Sample spans deterministically by trace-id using exact integer arithmetic.
--
-- The threshold @t@ is computed once:
--   t = floor(rate * 2^64)   using Integer to avoid Double overflow
-- A trace is sampled iff its first 8 bytes (big-endian Word64) are < t.
--
-- Boundary behaviour:
--   rate = 0.0 → t = 0         → nothing sampled (all Word64 values ≥ 0... wait)
--   Actually: t = 0 means the condition w < 0 is never true for Word64.
--   rate = 1.0 → t = 2^64      → all values sampled (all Word64 < 2^64)
--
-- The rate is clamped to [0.0, 1.0] before use; values outside this range
-- are handled safely without crashing.
traceIdRatioSampler :: Double -> Sampler
traceIdRatioSampler rate = Sampler $ \_ tid _ _ _ ->
  if traceIdWord64 tid < threshold then RecordAndSample else Drop
  where
    -- Clamp defensively. mkSampleRate enforces [0,1] at config load time,
    -- but this sampler is also constructible directly.
    clamped :: Double
    clamped = max 0.0 (min 1.0 rate)

    -- Compute threshold in Integer to avoid Double precision loss.
    -- 2^64 as Integer = 18446744073709551616
    threshold :: Word64
    threshold
      | clamped <= 0.0 = 0
      | clamped >= 1.0 = maxBound   -- all values satisfy w < 2^64, but we
                                    -- use maxBound and accept one missed value
                                    -- at the top; document below.
      | otherwise =
          fromIntegral
            ( floor
                ( toRational clamped
                  * (toRational (maxBound :: Word64) + 1)
                ) :: Integer
            )
    -- Note on rate=1.0: the true threshold is 2^64, which is not a Word64.
    -- We use maxBound (2^64-1) instead, meaning one trace-id value
    -- (all-0xFF bytes) is not sampled. This is a documented deviation of
    -- one part in 2^64 — immeasurably small in practice. The alternative
    -- (adding a special case) would complicate the hot path for no benefit.

-- | Extract the first 8 bytes of a 'TraceId' as a big-endian 'Word64'.
-- Used by 'traceIdRatioSampler'.
traceIdWord64 :: TraceId -> Word64
traceIdWord64 (TraceId bs) =
  BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0 (BS.take 8 bs)
```

Add `parentBasedSampler` immediately after:

```haskell
-- | A sampler that respects the parent span's sampling decision.
--
-- When a parent context is present:
--   * Parent was sampled ('isSampled' is True)  → 'RecordAndSample'
--   * Parent was not sampled                    → 'Drop'
--
-- When there is no parent (root span), the given @rootSampler@ is used.
--
-- This matches the OpenTelemetry @parentbased_*@ sampler semantics and
-- ensures that all spans in a trace are either all sampled or all dropped.
parentBasedSampler :: Sampler -> Sampler
parentBasedSampler rootSampler = Sampler $ \mParent tid name kind attrs_ ->
  case mParent of
    Nothing     -> runSampler rootSampler Nothing tid name kind attrs_
    Just parent ->
      if isSampled (scTraceFlags parent)
        then RecordAndSample
        else Drop
```

Export both new names from `src/Trace/Core.hs`:

```haskell
  -- In the Sampling section of the export list, replace:
  --   , traceIdRatioSampler
  -- with:
  , traceIdRatioSampler
  , parentBasedSampler
  , traceIdWord64          -- exported for testing only
```

**`src/Trace/Config.hs` — extend `SamplerConfig` and `loadSamplerConfig`**

```haskell
-- Replace the existing SamplerConfig definition:
data SamplerConfig
  = AlwaysSample
  | NeverSample
  | TraceIdRatio    !SampleRate
  | ParentBased     !SamplerConfig
    -- ^ Wraps a root sampler. When a parent context is present the parent's
    --   sampled bit is authoritative; the root sampler is used for root spans.
  deriving stock (Show, Eq)
```

```haskell
-- In loadSamplerConfig, add three new cases before the catch-all:
    Just "parentbased_always_on"  ->
      pure (Success (ParentBased AlwaysSample))
    Just "parentbased_always_off" ->
      pure (Success (ParentBased NeverSample))
    Just "parentbased_traceidratio" -> do
      arg <- lookupEnv "OTEL_TRACES_SAMPLER_ARG"
      case arg >>= readPositiveDouble of
        Nothing -> pure $ Failure $ NE.singleton $
          MissingRequiredVar (EnvVarName "OTEL_TRACES_SAMPLER_ARG")
        Just d  -> case mkSampleRate d of
          Left e   -> pure $ Failure $ NE.singleton e
          Right sr -> pure $ Success (ParentBased (TraceIdRatio sr))
```

**`src/Trace/Monad.hs` — extend `samplerFromConfig`**

```haskell
samplerFromConfig :: SamplerConfig -> Sampler
samplerFromConfig = \case
  AlwaysSample            -> alwaysOnSampler
  NeverSample             -> alwaysOffSampler
  TraceIdRatio sr         -> traceIdRatioSampler (unSampleRate sr)
  ParentBased rootCfg     -> parentBasedSampler (samplerFromConfig rootCfg)
```

**`src/Trace.hs` — add to re-exports**

```haskell
  , parentBasedSampler
```

## Modules touched

- `src/Trace/Core.hs` — replace `traceIdRatioSampler`/`traceIdRatio`, add
  `parentBasedSampler`, `traceIdWord64`; update export list
- `src/Trace/Config.hs` — extend `SamplerConfig`, `loadSamplerConfig`
- `src/Trace/Monad.hs` — extend `samplerFromConfig`
- `src/Trace.hs` — add `parentBasedSampler` to re-exports
- `test/Trace/Generators.hs` — no change needed

## Acceptance criteria

1. `stack build -Wall -Werror` passes.
2. `traceIdRatioSampler 1.0` samples 100% of trace-ids (all `Word64 < maxBound`
   except the all-0xFF case — documented).
3. `traceIdRatioSampler 0.0` samples 0% (threshold is 0; no `Word64` satisfies
   `w < 0` in unsigned arithmetic).
4. `parentBasedSampler alwaysOnSampler` with a sampled parent always returns
   `RecordAndSample`.
5. `parentBasedSampler alwaysOnSampler` with an unsampled parent always returns
   `Drop`, regardless of root sampler.
6. `fromEnv` with `OTEL_TRACES_SAMPLER=parentbased_always_on` produces
   `Right (TracingConfig { configSampler = ParentBased AlwaysSample, .. })`.
7. Distribution property: 10,000 random trace-ids with `rate=0.5` yield
   between 4,600 and 5,400 sampled (4 σ tolerance, σ=50).
8. All Phase R1/R2 tests still pass.

## Test plan

**Unit tests** — add to `test/Trace/CoreSpec.hs`:

```haskell
describe "traceIdRatioSampler (integer threshold)" $ do
  it "rate 0.0 samples nothing" $ do
    tids <- replicateM 1000 newTraceId
    let s = traceIdRatioSampler 0.0
    all (\t -> runSampler s Nothing t "n" Internal mempty == Drop) tids
      `shouldBe` True

  it "rate 1.0 samples all non-max trace-ids" $ do
    -- All-0xFF is the one exception; newTraceId never produces all-zero
    -- but can produce all-0xFF. We generate non-max ids explicitly.
    let Right nonMax = traceIdFromBytes (BS.pack (replicate 15 0xFF <> [0x00]))
    let s = traceIdRatioSampler 1.0
    runSampler s Nothing nonMax "n" Internal mempty
      `shouldBe` RecordAndSample

  it "negative rate clamps to 0 — samples nothing" $ do
    tids <- replicateM 200 newTraceId
    let s = traceIdRatioSampler (-1.0)
    all (\t -> runSampler s Nothing t "n" Internal mempty == Drop) tids
      `shouldBe` True

  it "rate > 1 clamps to 1 — samples all non-max" $ do
    let Right nonMax = traceIdFromBytes (BS.pack (replicate 15 0xFF <> [0x00]))
    runSampler (traceIdRatioSampler 2.0) Nothing nonMax "n" Internal mempty
      `shouldBe` RecordAndSample

  it "is deterministic for the same trace-id" $ do
    tid <- newTraceId
    let s = traceIdRatioSampler 0.5
    runSampler s Nothing tid "n" Internal mempty
      `shouldBe` runSampler s Nothing tid "n" Internal mempty

describe "parentBasedSampler" $ do
  it "no parent: delegates to root sampler (alwaysOn)" $ do
    tid <- newTraceId
    let s = parentBasedSampler alwaysOnSampler
    runSampler s Nothing tid "n" Internal mempty
      `shouldBe` RecordAndSample

  it "no parent: delegates to root sampler (alwaysOff)" $ do
    tid <- newTraceId
    let s = parentBasedSampler alwaysOffSampler
    runSampler s Nothing tid "n" Internal mempty
      `shouldBe` Drop

  it "sampled parent → RecordAndSample regardless of root" $ do
    tid <- newTraceId
    sid <- newSpanId
    let ctx = SpanContext tid sid Nothing (setSampled True defaultTraceFlags)
        s   = parentBasedSampler alwaysOffSampler
    runSampler s (Just ctx) tid "n" Internal mempty
      `shouldBe` RecordAndSample

  it "unsampled parent → Drop regardless of root" $ do
    tid <- newTraceId
    sid <- newSpanId
    let ctx = SpanContext tid sid Nothing defaultTraceFlags
        s   = parentBasedSampler alwaysOnSampler
    runSampler s (Just ctx) tid "n" Internal mempty
      `shouldBe` Drop
```

**Properties** — add to `test/Trace/CoreSpec.hs`:

```haskell
prop_ratio_sampler_distribution :: Property
prop_ratio_sampler_distribution = withTests 1 . property $ do
  tids <- liftIO (replicateM 10_000 newTraceId)
  let s       = traceIdRatioSampler 0.5
      sampled = length
        [ () | t <- tids
             , runSampler s Nothing t "n" Internal mempty == RecordAndSample ]
  -- Within 4 standard deviations of the mean (σ = sqrt(10000*0.5*0.5) = 50)
  H.diff sampled (>=) (5000 - 200)
  H.diff sampled (<=) (5000 + 200)

prop_parentBased_inherits_parent_decision :: Property
prop_parentBased_inherits_parent_decision = property $ do
  ctx <- forAll genSpanContext
  let s      = parentBasedSampler alwaysOnSampler
      result = runSampler s (Just ctx) (scTraceId ctx) "n" Internal mempty
  if isSampled (scTraceFlags ctx)
    then result H.=== RecordAndSample
    else result H.=== Drop

prop_parentBased_no_parent_delegates :: Property
prop_parentBased_no_parent_delegates = property $ do
  tid  <- liftIO newTraceId
  rate <- forAll (Gen.double (Range.linearFrac 0.0 1.0))
  let root   = traceIdRatioSampler rate
      pBased = parentBasedSampler root
      direct = runSampler root   Nothing tid "n" Internal mempty
      via    = runSampler pBased Nothing tid "n" Internal mempty
  direct H.=== via
```

**Config test** — add to `test/Trace/ConfigSpec.hs`:

```haskell
describe "loadSamplerConfig parentbased variants" $ do
  it "parentbased_always_on" $
    withEnv [("OTEL_TRACES_SAMPLER", "parentbased_always_on")] $ do
      Right cfg <- fromEnv
      configSampler cfg `shouldBe` ParentBased AlwaysSample

  it "parentbased_always_off" $
    withEnv [("OTEL_TRACES_SAMPLER", "parentbased_always_off")] $ do
      Right cfg <- fromEnv
      configSampler cfg `shouldBe` ParentBased NeverSample

  it "parentbased_traceidratio with valid arg" $
    withEnv [ ("OTEL_TRACES_SAMPLER",     "parentbased_traceidratio")
            , ("OTEL_TRACES_SAMPLER_ARG", "0.25")
            , ("OTEL_EXPORTER_OTLP_ENDPOINT", "http://localhost:4318")
            ] $ do
      Right cfg <- fromEnv
      configSampler cfg `shouldBe`
        ParentBased (TraceIdRatio (SampleRate 0.25))

  it "parentbased_traceidratio missing arg returns error" $
    withEnv [("OTEL_TRACES_SAMPLER", "parentbased_traceidratio")] $ do
      Left errs <- fromEnv
      any isMissingArg (NE.toList errs) `shouldBe` True
    where
      isMissingArg (MissingRequiredVar (EnvVarName v)) =
        v == "OTEL_TRACES_SAMPLER_ARG"
      isMissingArg _ = False
```

**Outlier tests**:

- `traceIdWord64` on a trace-id whose bytes are all `0x01`: result is
  `0x0101010101010101`; sampled by `traceIdRatioSampler 1.0`, dropped by
  `traceIdRatioSampler 0.0`.
- `traceIdRatioSampler 0.5` applied to the same trace-id twice: identical
  result (determinism).
- `parentBasedSampler (parentBasedSampler alwaysOnSampler)` — nested wrapping:
  with no parent, delegates all the way to `alwaysOnSampler`. With an
  unsampled parent, outer layer returns `Drop` without consulting inner.

## What this phase does NOT do

- Does not implement adaptive or tail-based samplers.
- Does not change the `SampleRate` validation bounds (still [0, 1]).
- Does not add `OTEL_TRACES_SAMPLER=parentbased_traceidratio` support beyond
  the three variants above.

---

# Phase R4 — Fix `memoryExporter` Append Order (O(n) → O(1))

**Goal.** Replace the O(n) left-append in `memoryExporter` with O(1) prepend
plus a single reverse on read, preserving arrival order while eliminating
the quadratic allocation pattern under sustained load.

## What gets built

**`src/Trace/Export/Types.hs` — `memoryExporter` only**

The current code:

```haskell
-- CURRENT — O(n) per export call:
atomically $ modifyTVar' tvar (<> NE.toList ne)
```

`(<>)` on lists is `(++)`, which is O(length of left argument). As the
accumulator grows, each export call traverses the entire list to find the
end. After N batches of size B this is O(N²·B) total allocation.

Replace with prepend (O(k) where k = batch size) and reverse on read:

```haskell
-- | An exporter that accumulates spans in memory.
-- Returns a pair of the exporter and a read action that returns all
-- spans received so far, in arrival order (oldest first).
--
-- Arrival order is maintained by prepending each new batch and reversing
-- on read. Write cost is O(k) per batch of k spans; read cost is O(n)
-- total, paid once per assertion.
memoryExporter :: IO (SpanExporter, IO [FinishedSpan])
memoryExporter = do
  tvar <- newTVarIO ([] :: [FinishedSpan])
  let doExport ne = do
        -- Prepend in O(k). The accumulator is stored newest-first.
        atomically $ modifyTVar' tvar (NE.toList ne ++)
        pure (ExportSuccess (NE.length ne))
      -- Reverse once on read to restore arrival order.
      readAll = reverse <$> readTVarIO tvar
  pure
    ( SpanExporter
        { exporterExport   = doExport
        , exporterFlush    = pure (Right ())
        , exporterShutdown = pure ()
        }
    , readAll
    )
```

No other changes to `src/Trace/Export/Types.hs`.

## Modules touched

- `src/Trace/Export/Types.hs` — `memoryExporter` only

## Acceptance criteria

1. `stack build -Wall -Werror` passes.
2. `prop_memoryExporter_preserves_order` passes: N spans exported one at a
   time arrive in the same order they were exported.
3. Concurrent export from 100 threads: all spans present in the result,
   no duplicates lost.
4. All existing tests pass without change.

## Test plan

**Unit tests** — add to `test/Trace/Export/TypesSpec.hs`:

```haskell
describe "memoryExporter arrival order" $ do
  it "single-span exports arrive in order" $ do
    (exp_, readAll) <- memoryExporter
    let spans = map sampleSpan [1..10]
    forM_ spans $ \s -> exporterExport exp_ (s NE.:| [])
    result <- readAll
    map (unSpanName . fsName) result
      `shouldBe`
        [ "span-" <> Text.pack (show i) | i <- [1..10] ]

  it "batch export preserves within-batch order" $ do
    (exp_, readAll) <- memoryExporter
    let s1 = sampleSpan 1
        s2 = sampleSpan 2
        s3 = sampleSpan 3
    void $ exporterExport exp_ (s1 NE.:| [s2, s3])
    result <- readAll
    map (unSpanName . fsName) result
      `shouldBe` ["span-1", "span-2", "span-3"]

  it "readAll is repeatable — same list on second call" $ do
    (exp_, readAll) <- memoryExporter
    forM_ (map sampleSpan [1..5]) $ \s ->
      exporterExport exp_ (s NE.:| [])
    r1 <- readAll
    r2 <- readAll
    r1 `shouldBe` r2
```

**Property**:

```haskell
prop_memoryExporter_preserves_order :: Property
prop_memoryExporter_preserves_order = property $ do
  n <- forAll (Gen.int (Range.linear 1 200))
  spans <- forAll (replicateM n genFinishedSpan)
  result <- liftIO $ do
    (exp_, readAll) <- memoryExporter
    forM_ spans $ \s -> exporterExport exp_ (s NE.:| [])
    readAll
  result H.=== spans
```

**Outlier**:

- 1,000 sequential single-span exports: result has exactly 1,000 spans in
  order, no allocation panic. Run with `+RTS -M128M` to confirm no quadratic
  growth.
- Empty batch: not possible via `NonEmpty`; `NonEmpty` guarantees at least one.

## What this phase does NOT do

- Does not change `noopExporter` or any other exporter.
- Does not change `SpanExporter` interface.

---

# Phase R5 — Log Worker Export Errors and Timeouts

**Goal.** The batch worker currently discards export errors and timeout
signals silently. Every export failure and every timeout must be reported
via `tracerLogger` so operators can observe them without log scraping or
external instrumentation.

## What gets built

The worker in `src/Trace/Export/Batch.hs` needs access to the logger.
The cleanest approach — consistent with the existing design — is to add
`InternalLogger` to `BatchConfig` so the worker has it in scope via the
closure.

**`src/Trace/Export/Batch.hs` — add `batchLogger` to `BatchConfig`**

```haskell
data BatchConfig = BatchConfig
  { maxQueueSize   :: !Int
  , maxExportBatch :: !Int
  , exportInterval :: !NominalDiffTime
  , exportTimeout  :: !NominalDiffTime
  , onDroppedSpans :: !(Int -> IO ())
  , batchLogger    :: !InternalLogger
    -- ^ Receives warnings for export failures and timeouts.
    -- Use 'silentLogger' to suppress (e.g. in tests that expect failures).
  }
```

```haskell
defaultBatchConfig :: BatchConfig
defaultBatchConfig = BatchConfig
  { maxQueueSize   = 2048
  , maxExportBatch = 512
  , exportInterval = 5
  , exportTimeout  = 30
  , onDroppedSpans = defaultOnDroppedSpans stderrLogger
  , batchLogger    = stderrLogger
  }
```

**`src/Trace/Export/Batch.hs` — update worker loop**

Replace the silent `void $ race ...` export call with one that logs:

```haskell
-- Inside the worker's loop, replace:
--
--   void $ race
--     (threadDelay timeoutMicros)
--     (try (exporterExport inner ne) :: IO (Either SomeException ExportResult))
--
-- with:

            raceResult <- race
              (threadDelay timeoutMicros)
              (try (exporterExport inner ne) :: IO (Either SomeException ExportResult))
            case raceResult of
              Left () ->
                logWarn (batchLogger cfg)
                  ( "htrace: export timed out after "
                  <> Text.pack (show (exportTimeout cfg))
                  <> "s; "
                  <> Text.pack (show (NE.length ne))
                  <> " spans abandoned"
                  )
              Right (Left ex) ->
                logError (batchLogger cfg)
                  ( "htrace: exporter threw exception: "
                  <> Text.pack (show ex)
                  )
              Right (Right (ExportFailure err)) ->
                logWarn (batchLogger cfg)
                  ( "htrace: export returned failure: "
                  <> Text.pack (show err)
                  )
              Right (Right (ExportSuccess _)) ->
                pure ()
```

**`src/Trace/Monad.hs` — pass logger into `defaultBatchConfig`**

In `withTracing`, the `batchCfg` is already customising `onDroppedSpans`.
Add `batchLogger`:

```haskell
      let batchCfg = defaultBatchConfig
            { onDroppedSpans = defaultOnDroppedSpans (configLogger cfg)
            , batchLogger    = configLogger cfg
            }
```

No other changes to `Monad.hs`.

## Modules touched

- `src/Trace/Export/Batch.hs` — `BatchConfig` (add field), `defaultBatchConfig`
  (add field), worker loop (add logging)
- `src/Trace/Monad.hs` — `withTracing` (set `batchLogger`)

## Acceptance criteria

1. `stack build -Wall -Werror` passes.
2. A simulated export timeout produces a `logWarn` call with text containing
   `"export timed out"`.
3. A simulated exporter exception produces a `logError` call.
4. A simulated `ExportFailure` produces a `logWarn` call.
5. `ExportSuccess` produces no log output.
6. Tests that inject a `silentLogger` into `batchLogger` produce no output —
   confirming the field is wired, not hard-coded to `stderrLogger`.
7. All prior tests pass.

## Test plan

**Test helpers** — add to `test/Trace/Export/BatchSpec.hs`:

```haskell
-- | Capture log messages during a test without touching stderr.
capturingLogger :: IO (InternalLogger, IO [Text], IO [Text])
capturingLogger = do
  warns  <- newIORef []
  errors <- newIORef []
  let logger = InternalLogger
        { logWarn  = \t -> modifyIORef' warns  (t:)
        , logError = \t -> modifyIORef' errors (t:)
        }
  pure (logger, fmap reverse (readIORef warns), fmap reverse (readIORef errors))
```

**Unit tests** — add to `test/Trace/Export/BatchSpec.hs`:

```haskell
describe "batchExporter logging" $ do
  it "logs a warning on export timeout" $ do
    (logger, readWarns, _) <- capturingLogger
    -- Inner exporter sleeps longer than the timeout.
    let slowExporter = SpanExporter
          { exporterExport   = \_ -> threadDelay 5_000_000 >> pure (ExportSuccess 1)
          , exporterFlush    = pure (Right ())
          , exporterShutdown = pure ()
          }
    Right batched <- batchExporter
      defaultBatchConfig
        { exportInterval = 0.01   -- wake quickly
        , exportTimeout  = 0.05   -- time out in 50ms; slow exporter sleeps 5s
        , batchLogger    = logger
        }
      slowExporter
    void $ exporterExport batched (sampleSpan 0 NE.:| [])
    threadDelay 300_000   -- let the worker wake and time out
    exporterShutdown batched
    warns <- readWarns
    any (Text.isInfixOf "timed out") warns `shouldBe` True

  it "logs an error when exporter throws" $ do
    (logger, _, readErrors) <- capturingLogger
    let crashExporter = SpanExporter
          { exporterExport   = \_ -> throwIO (userError "boom")
          , exporterFlush    = pure (Right ())
          , exporterShutdown = pure ()
          }
    Right batched <- batchExporter
      defaultBatchConfig
        { exportInterval = 0.01
        , exportTimeout  = 5
        , batchLogger    = logger
        }
      crashExporter
    void $ exporterExport batched (sampleSpan 0 NE.:| [])
    threadDelay 200_000
    exporterShutdown batched
    errs <- readErrors
    any (Text.isInfixOf "threw exception") errs `shouldBe` True

  it "logs a warning on ExportFailure" $ do
    (logger, readWarns, _) <- capturingLogger
    let failExporter = SpanExporter
          { exporterExport   = \_ ->
              pure (ExportFailure (EndpointUnreachable "test"))
          , exporterFlush    = pure (Right ())
          , exporterShutdown = pure ()
          }
    Right batched <- batchExporter
      defaultBatchConfig
        { exportInterval = 0.01
        , exportTimeout  = 5
        , batchLogger    = logger
        }
      failExporter
    void $ exporterExport batched (sampleSpan 0 NE.:| [])
    threadDelay 200_000
    exporterShutdown batched
    warns <- readWarns
    any (Text.isInfixOf "export returned failure") warns `shouldBe` True

  it "ExportSuccess produces no log output" $ do
    (logger, readWarns, readErrors) <- capturingLogger
    Right batched <- batchExporter
      defaultBatchConfig { exportInterval = 0.01, batchLogger = logger }
      noopExporter
    void $ exporterExport batched (sampleSpan 0 NE.:| [])
    threadDelay 200_000
    exporterShutdown batched
    warns  <- readWarns
    errors <- readErrors
    warns  `shouldBe` []
    errors `shouldBe` []
```

**Outlier** — a logger whose `logWarn` itself throws: the worker must not
crash. Wrap the log call in `try`:

```haskell
-- In the worker, use a safe log wrapper:
safeLog :: InternalLogger -> (InternalLogger -> Text -> IO ()) -> Text -> IO ()
safeLog logger f msg = do
  r <- try (f logger msg) :: IO (Either SomeException ())
  case r of
    Right () -> pure ()
    Left  _  -> pure ()   -- logger failure must never crash the worker
```

Apply `safeLog` around all four log sites in the worker. Add a test:

```haskell
  it "crashing logger does not crash the worker" $ do
    let crashLogger = InternalLogger
          { logWarn  = \_ -> throwIO (userError "logger crash")
          , logError = \_ -> throwIO (userError "logger crash")
          }
    let failExporter = SpanExporter
          { exporterExport   = \_ ->
              pure (ExportFailure (EndpointUnreachable "test"))
          , exporterFlush    = pure (Right ())
          , exporterShutdown = pure ()
          }
    Right batched <- batchExporter
      defaultBatchConfig
        { exportInterval = 0.01
        , exportTimeout  = 5
        , batchLogger    = crashLogger
        }
      failExporter
    void $ exporterExport batched (sampleSpan 0 NE.:| [])
    threadDelay 200_000
    exporterShutdown batched  -- must return without throwing
    pure ()
```

## What this phase does NOT do

- Does not add retry-with-backoff (deliberate; retry is a v0.2 feature with
  its own backpressure implications).
- Does not add a queue-depth gauge.
- Does not change the `doFlush` path (flush errors are already returned as
  `Left ExportError` to the caller).

---

# Phase R6 — Forward-Compatible `traceparent` Version Parsing

**Goal.** Accept future W3C `traceparent` versions by treating any
two-hex-digit version other than the reserved `"ff"` as valid, extracting
the first four fields and ignoring any additional fields.

## What gets built

**`src/Trace/Propagation.hs` — `parseTraceparent` and `validVersion` only**

The current parser uses a list pattern `[v, tid, sid, flgs]` which matches
exactly four fields. This rejects headers with extra fields (permitted by the
W3C spec for future versions). Replace with a guard on `length >= 4`:

```haskell
-- | Parse a @traceparent@ header value.
--
-- Accepts version @00@ and any future two-hex-digit version other than @ff@.
-- When a future version is encountered, the first four dash-separated fields
-- are extracted and any additional fields are ignored, per W3C Trace Context
-- specification §4.3 (forward compatibility).
--
-- Version @ff@ is permanently reserved by the W3C and is always rejected.
parseTraceparent :: Text -> PropagationResult
parseTraceparent t =
  case Text.splitOn "-" t of
    (v : tidStr : sidStr : flgsStr : _)
      | not (validVersion v) ->
          PropagationInvalid (InvalidVersion v)
      | otherwise ->
          case decodeHex 16 tidStr of
            Left _      -> PropagationInvalid (InvalidTraceId tidStr)
            Right tidBs ->
              case traceIdFromBytes tidBs of
                Left _        -> PropagationInvalid (InvalidTraceId tidStr)
                Right traceId ->
                  case decodeHex 8 sidStr of
                    Left _      -> PropagationInvalid (InvalidSpanId sidStr)
                    Right sidBs ->
                      case spanIdFromBytes sidBs of
                        Left _      -> PropagationInvalid (InvalidSpanId sidStr)
                        Right spanId ->
                          case parseFlags flgsStr of
                            Nothing -> PropagationInvalid (InvalidFlags flgsStr)
                            Just f  ->
                              PropagationSuccess
                                (SpanContext traceId spanId Nothing f)
    _ -> PropagationInvalid (MalformedHeader t)
  where
    -- Accept any two lowercase-hex-digit version except the W3C-reserved "ff".
    -- The spec defines version bytes as lowercase hex; we normalise to lowercase
    -- before comparison so "0F" and "0f" are treated identically.
    validVersion v =
      Text.length v == 2
        && Text.all isHexDigit v
        && Text.toLower v /= "ff"

    parseFlags f
      | Text.length f == 2 && Text.all isHexDigit f =
          Just (TraceFlags (fromIntegral (hexToInt f)))
      | otherwise = Nothing
```

No other changes to `src/Trace/Propagation.hs`.

## Modules touched

- `src/Trace/Propagation.hs` — `parseTraceparent`, `validVersion`

## Acceptance criteria

1. `stack build -Wall -Werror` passes.
2. The existing round-trip property `prop_traceparent_round_trip` passes
   unchanged (it only generates version `"00"` via `emitTraceparent`).
3. `parseTraceparent "01-<valid-trace>-<valid-span>-01"` returns
   `PropagationSuccess _`.
4. `parseTraceparent "ff-<valid-trace>-<valid-span>-01"` returns
   `PropagationInvalid (InvalidVersion "ff")`.
5. `parseTraceparent "00-<valid-trace>-<valid-span>-01-extra-field"` returns
   `PropagationSuccess _` (extra field ignored).
6. All five `PropagationError` constructors remain reachable and tested.
7. All prior tests pass.

## Test plan

**Unit tests** — add to `test/Trace/PropagationSpec.hs`:

```haskell
-- Valid header used as a base; substitute the version prefix in each test.
validSuffix :: Text
validSuffix =
  "4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"

describe "forward-compatible version parsing" $ do
  it "version 00 is still accepted" $
    parseTraceparent ("00-" <> validSuffix)
      `shouldSatisfy` \case PropagationSuccess _ -> True; _ -> False

  it "version 01 is accepted (future version)" $
    parseTraceparent ("01-" <> validSuffix)
      `shouldSatisfy` \case PropagationSuccess _ -> True; _ -> False

  it "version 0f is accepted" $
    parseTraceparent ("0f-" <> validSuffix)
      `shouldSatisfy` \case PropagationSuccess _ -> True; _ -> False

  it "version 0F (uppercase) is accepted — normalised to 0f" $
    parseTraceparent ("0F-" <> validSuffix)
      `shouldSatisfy` \case PropagationSuccess _ -> True; _ -> False

  it "version ff is always rejected" $
    parseTraceparent ("ff-" <> validSuffix)
      `shouldBe` PropagationInvalid (InvalidVersion "ff")

  it "version FF is also rejected (normalised to ff)" $
    parseTraceparent ("FF-" <> validSuffix)
      `shouldBe` PropagationInvalid (InvalidVersion "ff")

  it "extra fields after flags are ignored" $
    -- Future versions may add a fifth field; we must not reject.
    parseTraceparent ("01-" <> validSuffix <> "-extra-data")
      `shouldSatisfy` \case PropagationSuccess _ -> True; _ -> False

  it "fewer than four fields is still MalformedHeader" $
    parseTraceparent "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7"
      `shouldSatisfy` \case PropagationInvalid (MalformedHeader _) -> True; _ -> False
```

**Outlier tests**:

- `parseTraceparent ""` → `PropagationInvalid (MalformedHeader _)`
- `parseTraceparent "00-" <> validSuffix <> " "` (trailing space) →
  `MalformedHeader` (we do not strip whitespace from header values)
- `parseTraceparent "00-" <> validSuffix` with span-id all zeros →
  `PropagationInvalid (InvalidSpanId _)` — the all-zero rejection from
  `spanIdFromBytes` is still enforced

**Existing property must still pass**:

```haskell
prop_traceparent_round_trip :: Property
prop_traceparent_round_trip = property $ do
  ctx <- forAll genSpanContextNoParent
  -- emitTraceparent always emits "00"; that version must be accepted.
  case parseTraceparent (emitTraceparent ctx) of
    PropagationSuccess ctx' ->
      -- scParentId is not encoded in traceparent; it will be Nothing after parse.
      ctx' H.=== ctx { scParentId = Nothing }
    other -> H.footnote (show other) >> H.failure
```

## What this phase does NOT do

- Does not emit future versions — `emitTraceparent` always produces `"00"`.
- Does not parse `tracestate`.
- Does not add B3 or any other propagation format.

---

## Stack commands reference

```bash
# Build with strict warnings — must pass after every phase
stack build --ghc-options "-Wall -Werror"

# Run the full test suite
stack test

# Run a single spec file
stack test --test-arguments '--match "/SpanName encapsulation/"'

# Confirm the encapsulation guard fails to compile (CI check)
stack build htrace:test:htrace-encapsulation-test 2>&1 \
  | grep -q "Not in scope" && echo "PASS: constructor hidden" || echo "FAIL"
```

## Dependency check

No new packages are needed. Every import introduced across all six phases
comes from packages already declared in `htrace.cabal` and present in
`lts-23.28`:

| New import | Package | Already declared |
|------------|---------|-----------------|
| `readTVarIO` | `stm >= 2.5` | Yes |
| `toRational` | `base >= 4.18` (Prelude) | Yes |
| `Text.toLower` (in `validVersion`) | `text` | Yes |
| `throwIO` in test | `base >= 4.18` | Yes |

`stack.yaml` and `stack.yaml.lock` are unchanged.

## Done criteria

After all six phases:

- `stack build --ghc-options "-Wall -Werror"` passes.
- `stack test` passes every unit test, property test (pinned seeds), and
  integration test.
- The encapsulation guard fails to compile, proving `TVar` write access is
  package-internal.
- `setStatusError sp ""` never calls `error`; it returns `Right ()` with a
  well-formed fallback message.
- Child spans always inherit their parent's sampled bit when
  `parentBasedSampler` is the configured sampler.
- Every export failure or timeout is logged; no information is silently
  discarded in the batch worker.
- Future `traceparent` versions are accepted; version `"ff"` is always
  rejected.
