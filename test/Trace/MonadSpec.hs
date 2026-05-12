module Trace.MonadSpec (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import Data.Text qualified as Text
import Test.Hspec

import Trace.Attributes
import Trace.Core
import Trace.Export.Types
import Trace.Monad
import Trace.Generators

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

mkTestTracer :: SpanExporter -> Tracer
mkTestTracer exporter = Tracer
  { tracerScope    = InstrumentationScope "test" Nothing
  , tracerSampler  = alwaysOnSampler
  , tracerExporter = exporter
  , tracerClock    = systemClock
  , tracerLogger   = silentLogger
  }

withCapturedSpan
  :: (Tracer -> Span -> IO ())
  -> IO FinishedSpan
withCapturedSpan action = do
  (exporter, readAll) <- memoryExporter
  let tracer = mkTestTracer exporter
  inSpan tracer "test" Internal mempty (action tracer)
  spans <- readAll
  case spans of
    (s:_) -> pure s
    []    -> fail "no span was exported"

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "inSpan" $ do
    it "exports span on normal return" $ do
      (exporter, readAll) <- memoryExporter
      let tracer = mkTestTracer exporter
      inSpan tracer "t" Internal mempty (\_ -> pure ())
      spans <- readAll
      length spans `shouldBe` 1

    it "exports span when body throws" $ do
      (exporter, readAll) <- memoryExporter
      let tracer = mkTestTracer exporter
      _ <- (inSpan tracer "t" Internal mempty
              (\_ -> fail "boom" :: IO ()) :: IO ())
             `shouldThrow` anyException
      spans <- readAll
      length spans `shouldBe` 1

    it "re-raises exception after finalising" $ do
      (exporter, _) <- memoryExporter
      let tracer = mkTestTracer exporter
      let action = inSpan tracer "t" Internal mempty
                     (\_ -> ioError (userError "boom"))
      action `shouldThrow` anyIOException

    it "span start time <= end time" $ do
      fs <- withCapturedSpan (\_ _ -> pure ())
      fsStartTime fs `shouldSatisfy` (<= fsEndTime fs)

    it "alwaysOffSampler produces no exported spans" $ do
      (exporter, readAll) <- memoryExporter
      let tracer = (mkTestTracer exporter)
                     { tracerSampler = alwaysOffSampler }
      inSpan tracer "t" Internal mempty (\_ -> pure ())
      spans <- readAll
      length spans `shouldBe` 0

  describe "setSpanAttr" $ do
    it "sets an attribute on a live span" $ do
      fs <- withCapturedSpan $ \_ sp -> do
        _ <- setSpanAttr sp (AttrKey "k") (AttrString "v")
        pure ()
      lookupAttr (AttrKey "k") (fsAttributes fs)
        `shouldBe` Right (AttrString "v")

    it "returns Left SpanAlreadyEnded after span ends" $ do
      (exporter, _) <- memoryExporter
      let tracer = mkTestTracer exporter
      spRef <- newIORef (error "uninitialised")
      inSpan tracer "t" Internal mempty $ \sp -> writeIORef spRef sp
      sp <- readIORef spRef
      result <- setSpanAttr sp (AttrKey "k") (AttrString "v")
      result `shouldBe` Left SpanAlreadyEnded

    it "returns Left SpanWasDropped on dropped span" $ do
      (exporter, _) <- memoryExporter
      let tracer = (mkTestTracer exporter)
                     { tracerSampler = alwaysOffSampler }
      result <- inSpan tracer "t" Internal mempty $ \sp ->
        setSpanAttr sp (AttrKey "k") (AttrString "v")
      result `shouldBe` Left SpanWasDropped

  describe "setSpanAttrs" $ do
    it "merges multiple attributes" $ do
      fs <- withCapturedSpan $ \_ sp -> do
        _ <- setSpanAttrs sp
               [ (AttrKey "a", AttrInt 1)
               , (AttrKey "b", AttrInt 2) ]
        pure ()
      lookupAttr (AttrKey "a") (fsAttributes fs) `shouldBe` Right (AttrInt 1)
      lookupAttr (AttrKey "b") (fsAttributes fs) `shouldBe` Right (AttrInt 2)

  describe "setStatusError" $ do
    it "sets StatusError with message" $ do
      fs <- withCapturedSpan $ \_ sp -> do
        _ <- setStatusError sp "something failed"
        pure ()
      case fsStatus fs of
        StatusError em ->
          unErrorMessage em `shouldBe` "something failed"
        other ->
          expectationFailure ("expected StatusError, got: " <> show other)

    it "falls back to <unspecified error> for blank message" $ do
      fs <- withCapturedSpan $ \_ sp -> do
        _ <- setStatusError sp "   "
        pure ()
      case fsStatus fs of
        StatusError em ->
          unErrorMessage em `shouldBe` "<unspecified error>"
        other ->
          expectationFailure ("expected StatusError, got: " <> show other)

  describe "addEvent" $ do
    it "appends events in chronological order" $ do
      fs <- withCapturedSpan $ \_ sp -> do
        _ <- addEvent sp "first"  mempty
        _ <- addEvent sp "second" mempty
        _ <- addEvent sp "third"  mempty
        pure ()
      map eventName (fsEvents fs) `shouldBe` ["first", "second", "third"]

    it "returns Left SpanAlreadyEnded after span ends" $ do
      (exporter, _) <- memoryExporter
      let tracer = mkTestTracer exporter
      spRef <- newIORef (error "uninitialised")
      inSpan tracer "t" Internal mempty $ \sp -> writeIORef spRef sp
      sp <- readIORef spRef
      result <- addEvent sp "late" mempty
      result `shouldBe` Left SpanAlreadyEnded

  describe "recordException" $ do
    it "sets exception.type and exception.message attributes on event" $ do
      fs <- withCapturedSpan $ \_ sp ->
        void $ recordException sp (userError "boom")
      let ev = case fsEvents fs of
                 (e:_) -> e
                 []    -> error "expected at least one event"
      eventName ev `shouldBe` "exception"
      lookupAttr (AttrKey "exception.message") (eventAttributes ev)
        `shouldSatisfy` (\r -> case r of
          Right (AttrString m) -> "boom" `Text.isInfixOf` m
          _                    -> False)

    it "sets span status to StatusError" $ do
      fs <- withCapturedSpan $ \_ sp ->
        void $ recordException sp (userError "boom")
      case fsStatus fs of
        StatusError _ -> pure ()
        other         ->
          expectationFailure ("expected StatusError, got: " <> show other)

    it "sets both status and event atomically — both present or neither" $ do
      fs <- withCapturedSpan $ \_ sp ->
        void $ recordException sp (userError "atomic-test")
      -- Both must be present. If non-atomic, a race could produce one
      -- without the other.
      case fsStatus fs of
        StatusError _ -> pure ()
        other ->
          expectationFailure ("expected StatusError, got: " <> show other)
      case fsEvents fs of
        [ev] -> eventName ev `shouldBe` "exception"
        []   -> expectationFailure "expected exception event but got none"
        evs  -> expectationFailure
          ("expected exactly 1 event, got " <> show (length evs))

    it "returns Left SpanAlreadyEnded when span has ended" $ do
      (exporter, _) <- memoryExporter
      let tracer = mkTestTracer exporter
      spRef <- newIORef (error "uninitialised")
      inSpan tracer "t" Internal mempty $ \sp -> writeIORef spRef sp
      sp <- readIORef spRef
      result <- recordException sp (userError "too late")
      result `shouldBe` Left SpanAlreadyEnded      

  describe "inSpanM" $ do
    it "child span inherits trace ID from parent" $ do
      (exporter, readAll) <- memoryExporter
      let tracer = mkTestTracer exporter
          ctx    = TraceContext Nothing tracer
      runReaderT
        ( inSpanM "parent" Internal mempty $ \_ ->
            inSpanM "child" Internal mempty $ \_ -> pure ()
        )
        ctx
      spans <- readAll
      length spans `shouldBe` 2
      let traceIds = map (scTraceId . fsContext) spans
      case traceIds of
        (t:ts) -> length (filter (== t) ts) `shouldBe` 1
        []     -> expectationFailure "no spans exported"

    it "child span has correct parent ID" $ do
      (exporter, readAll) <- memoryExporter
      let tracer = mkTestTracer exporter
          ctx    = TraceContext Nothing tracer
      runReaderT
        ( inSpanM "parent" Internal mempty $ \parent ->
            inSpanM "child" Internal mempty $ \_ -> do
              ambientCtx <- getCurrentSpanContext
              liftIO $ case ambientCtx of
                Nothing -> expectationFailure "expected a span context"
                Just c  ->
                  scParentId c
                    `shouldBe` Just (scSpanId (spanContext parent))
        )
        ctx
      spans <- readAll
      (child, parent) <- case spans of
        [c, p] -> pure (c, p)
        _      -> fail ("expected 2 spans, got " <> show (length spans))
      scParentId (fsContext child)
        `shouldBe` Just (scSpanId (fsContext parent))

  describe "flush" $ do
    it "returns Right () on a noop exporter" $ do
      let tracer = mkTestTracer noopExporter
      result <- flush tracer
      result `shouldBe` Right ()

  describe "properties" $ do
    it "inSpan always produces start <= end" $
      prop_inSpan_start_le_end

    it "setSpanAttr acknowledged writes survive in snapshot" $
      prop_setSpanAttr_atomic

  -- -------------------------------------------------------------------------
  -- Phase R1: readSpanInternals and SpanName encapsulation
  -- -------------------------------------------------------------------------

  describe "readSpanInternals" $ do
    it "returns SpanActive while span is open" $ do
      (exporter, _) <- memoryExporter
      let tracer = mkTestTracer exporter
      inSpan tracer "r1-active" Internal mempty $ \sp -> do
        si <- readSpanInternals sp
        case siState si of
          SpanActive _ -> pure ()
          other        -> expectationFailure
            ("expected SpanActive, got: " <> show other)

    it "reflects setSpanAttr immediately" $ do
      (exporter, _) <- memoryExporter
      let tracer = mkTestTracer exporter
      inSpan tracer "r1-attr" Internal mempty $ \sp -> do
        result <- setSpanAttr sp (AttrKey "key1") (AttrString "val1")
        result `shouldBe` Right ()
        si <- readSpanInternals sp
        lookupAttr (AttrKey "key1") (siAttributes si)
          `shouldBe` Right (AttrString "val1")

    it "shows exported span has StatusUnset when no status set" $ do
      (exporter, readAll) <- memoryExporter
      let tracer = mkTestTracer exporter
      inSpan tracer "r1-ended" Internal mempty (\_ -> pure ())
      spans <- readAll
      case spans of
        [fs] -> fsStatus fs `shouldBe` StatusUnset
        _    -> fail ("expected exactly 1 span, got " <> show (length spans))

  describe "SpanName encapsulation" $ do
    it "mkSpanName rejects empty text" $
      mkSpanName "" `shouldBe` Nothing

    it "mkSpanName rejects whitespace-only text" $
      mkSpanName "   " `shouldBe` Nothing

    it "mkSpanName accepts non-blank text" $
      mkSpanName "checkout" `shouldBe` Just (SpanName "checkout")

    it "IsString instance falls back for empty string literal" $
      unSpanName ("" :: SpanName) `shouldBe` "<unnamed-span>"

    it "IsString instance falls back for whitespace-only literal" $
      unSpanName ("   " :: SpanName) `shouldBe` "<unnamed-span>"

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

prop_inSpan_start_le_end :: IO ()
prop_inSpan_start_le_end = do
  (exporter, readAll) <- memoryExporter
  let tracer = mkTestTracer exporter
  inSpan tracer "t" Internal mempty (\_ -> pure ())
  spans <- readAll
  case spans of
    [fs] -> fsStartTime fs `shouldSatisfy` (<= fsEndTime fs)
    _    -> fail "expected exactly one span"

prop_setSpanAttr_atomic :: IO ()
prop_setSpanAttr_atomic = do
  let n = 20
  (exporter, readAll) <- memoryExporter
  let tracer = mkTestTracer exporter
  ackedKeys <- newIORef ([] :: [AttrKey])
  inSpan tracer "t" Internal mempty $ \sp -> do
    mvars <- sequence (replicate n newEmptyMVar)
    mapM_
      ( \(i, mv) -> forkIO $ do
          let k = AttrKey ("k" <> Text.pack (show (i :: Int)))
          r <- setSpanAttr sp k (AttrInt (fromIntegral i))
          case r of
            Right () -> modifyIORef' ackedKeys (k :)
            Left  _  -> pure ()
          putMVar mv ()
      )
      (zip [0..] mvars)
    mapM_ takeMVar mvars
  acked <- readIORef ackedKeys
  spans <- readAll
  case spans of
    [fs] ->
      mapM_
        ( \k ->
            lookupAttr k (fsAttributes fs)
              `shouldNotBe` Left (MissingAttr k)
        )
        acked
    _ -> fail "expected exactly one span"