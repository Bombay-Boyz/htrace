module BatchBench
  ( benchmarks
  ) where

import Control.Concurrent (threadDelay)
import Data.List.NonEmpty qualified as NE
import Data.IORef
import Test.Tasty.Bench

import Trace.Core
import Trace.Export.Batch
import Trace.Export.Types
import Trace.Propagation (parseTraceparent)
import Data.Text qualified as Text

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | A batch config tuned for benchmarking: very long interval so the
-- worker never wakes spontaneously; all flushing is explicit.
benchConfig :: BatchConfig
benchConfig = defaultBatchConfig
  { maxQueueSize   = 4096
  , maxExportBatch = 512
  , exportInterval = 3600   -- never wakes on timer during a bench run
  , exportTimeout  = 30
  , onDroppedSpans = \_ -> pure ()
  , batchLogger    = silentLogger
  }

-- | Make a trivial finished span for use as benchmark payload.
-- We reuse the same value to keep allocation noise out of the
-- measurements we care about.
sampleSpan :: FinishedSpan
sampleSpan = FinishedSpan
  { fsContext    = SpanContext
      { scTraceId    = either (error "bench: bad traceId") id
                         (traceIdFromBytes "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f")
      , scSpanId     = either (error "bench: bad spanId") id
                         (spanIdFromBytes "\x00\x01\x02\x03\x04\x05\x06\x07")
      , scParentId   = Nothing
      , scTraceFlags = defaultTraceFlags
      }
  , fsName       = SpanName "bench-span"
  , fsKind       = Internal
  , fsStartTime  = read "2024-01-01 00:00:00 UTC"
  , fsEndTime    = read "2024-01-01 00:00:01 UTC"
  , fsStatus     = StatusOk
  , fsAttributes = mempty
  , fsEvents     = []
  }

-- | Right-biased setup: crash on Left so bench failures are obvious.
setup :: IO SpanExporter
setup = do
  (inner, _) <- memoryExporter
  Right batched <- batchExporter benchConfig inner
  pure batched

-- ---------------------------------------------------------------------------
-- Benchmark groups
-- ---------------------------------------------------------------------------

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup "enqueue"
      [ bench "single span" $ whnfIO $ do
          batched <- setup
          _ <- exporterExport batched (sampleSpan NE.:| [])
          exporterShutdown batched

      , bench "100 spans sequential" $ whnfIO $ do
          batched <- setup
          mapM_ (\_ -> exporterExport batched (sampleSpan NE.:| []))
                [1 .. 100 :: Int]
          exporterShutdown batched

      , bench "512 spans sequential (full batch)" $ whnfIO $ do
          batched <- setup
          mapM_ (\_ -> exporterExport batched (sampleSpan NE.:| []))
                [1 .. 512 :: Int]
          exporterShutdown batched
      ]

  , bgroup "flush"
      [ bench "flush empty queue" $ whnfIO $ do
          batched <- setup
          _ <- exporterFlush batched
          exporterShutdown batched

      , bench "flush 100 buffered spans" $ whnfIO $ do
          batched <- setup
          mapM_ (\_ -> exporterExport batched (sampleSpan NE.:| []))
                [1 .. 100 :: Int]
          _ <- exporterFlush batched
          exporterShutdown batched

      , bench "flush 512 buffered spans" $ whnfIO $ do
          batched <- setup
          mapM_ (\_ -> exporterExport batched (sampleSpan NE.:| []))
                [1 .. 512 :: Int]
          _ <- exporterFlush batched
          exporterShutdown batched
      ]

  , bgroup "shutdown"
      [ bench "shutdown empty queue" $ whnfIO $ do
          batched <- setup
          exporterShutdown batched

      , bench "shutdown draining 200 spans" $ whnfIO $ do
          batched <- setup
          mapM_ (\_ -> exporterExport batched (sampleSpan NE.:| []))
                [1 .. 200 :: Int]
          exporterShutdown batched
      ]

  , bgroup "drop"
      [ -- Measure enqueue cost when queue is full (drop path).
        bench "enqueue into full queue (drop path)" $ whnfIO $ do
          droppedRef <- newIORef (0 :: Int)
          let cfg = benchConfig
                { maxQueueSize   = 10
                , maxExportBatch = 10
                , onDroppedSpans = \n -> modifyIORef' droppedRef (+ n)
                }
          Right batched <- batchExporter cfg noopExporter
          -- Fill the queue first.
          mapM_ (\_ -> exporterExport batched (sampleSpan NE.:| []))
                [1 .. 10 :: Int]
          -- This enqueue hits the drop path.
          _ <- exporterExport batched (sampleSpan NE.:| [])
          exporterShutdown batched
      ]

  , bgroup "propagation"
      [ bench "parseTraceparent valid v00" $ whnfIO $
          pure $! parseTraceparent
            "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"

      , bench "parseTraceparent invalid (malformed)" $ whnfIO $
          pure $! parseTraceparent "not-a-traceparent"

      , bench "parseTraceparent 1000x" $ whnfIO $
          let t = "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
          in mapM_ (\_ -> pure $! parseTraceparent t) [1 .. 1000 :: Int]
      ]
  ]
