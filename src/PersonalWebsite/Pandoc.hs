{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PersonalWebsite.Pandoc (
    PandocP (..),
    runPandocIO,
    Render (..),
    tracePandoc,
    renderMarkdown,
    runRenderViaPandoc,
    renderWithCache,
    traceRender,
) where

import qualified Control.Monad.Except as CE
import qualified Data.Time as T
import Katip
import qualified OpenTelemetry.Trace as Otel
import Optics
import PersonalWebsite.Tracing
import Polysemy
import Polysemy.Error
import Polysemy.KVStore
import Polysemy.State
import Relude hiding (State, get, gets, lookupEnv, modify, put, trace)
import qualified System.Random as R
import Text.Blaze.Html
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Highlighting as P
import Text.Pandoc.MIME

data PandocP m a where
    LookupEnv :: Text -> PandocP m (Maybe Text)
    GetCurrentTime :: PandocP m T.UTCTime
    GetCurrentTimeZone :: PandocP m T.TimeZone
    NewStdGen :: PandocP m R.StdGen
    NewUniqueHash :: PandocP m Int
    OpenURL :: Text -> PandocP m (ByteString, Maybe MimeType)
    ReadFileLazy :: FilePath -> PandocP m LByteString
    ReadFileStrict :: FilePath -> PandocP m ByteString
    ReadStdinStrict :: PandocP m ByteString
    Glob :: String -> PandocP m [FilePath]
    FileExists :: FilePath -> PandocP m Bool
    GetDataFileName :: FilePath -> PandocP m FilePath
    GetModificationTime :: FilePath -> PandocP m T.UTCTime
    GetCommonState :: PandocP m P.CommonState
    PutCommonState :: P.CommonState -> PandocP m ()
    GetsCommonState :: (P.CommonState -> a) -> PandocP m a
    ModifyCommonState :: (P.CommonState -> P.CommonState) -> PandocP m ()
    LogOutput :: P.LogMessage -> PandocP m ()
    Trace :: Text -> PandocP m ()

makeSem ''PandocP

runPandocIO ::
    forall r a.
    (Members '[Error P.PandocError, Embed IO, State P.CommonState] r, KatipContext (Sem r)) =>
    Map Text Text ->
    Sem (PandocP ': r) a ->
    Sem r a
runPandocIO env = interpret \case
    LookupEnv e -> pure $ env ^? ix e
    GetCurrentTime -> embed T.getCurrentTime
    GetCurrentTimeZone -> embed T.getCurrentTimeZone
    NewStdGen -> embed @IO R.newStdGen
    NewUniqueHash -> viaIO P.newUniqueHash
    OpenURL u -> viaIO $ P.openURL u
    ReadFileLazy f -> viaIO $ P.readFileLazy f
    ReadFileStrict f -> viaIO $ P.readFileStrict f
    ReadStdinStrict -> viaIO P.readStdinStrict
    Glob g -> viaIO $ P.glob g
    FileExists f -> viaIO $ P.fileExists f
    GetDataFileName f -> viaIO $ P.getDataFileName f
    GetModificationTime f -> viaIO $ P.getModificationTime f
    GetCommonState -> get
    GetsCommonState f -> gets f
    PutCommonState s -> put s
    ModifyCommonState f -> modify f
    LogOutput msg -> katipAddContext (sl "data" msg) $ $(logTM) DebugS "pandoc message"
    Trace t -> $(logTM) DebugS (logStr t)
  where
    viaIO :: forall b. P.PandocIO b -> Sem r b
    viaIO = either throw pure <=< (liftIO . P.runIO)

tracePandoc :: Members [PandocP, Tracing] r => Sem r a -> Sem r a
tracePandoc = intercept \case
    LookupEnv e ->
        inSpan'
            "LookupEnv"
            Otel.defaultSpanArguments{Otel.attributes = one ("key", Otel.toAttribute e)}
            $ lookupEnv e
    GetCurrentTime -> inSpan' "GetCurrentTime" Otel.defaultSpanArguments getCurrentTime
    GetCurrentTimeZone ->
        inSpan' "GetCurrentTimeZone" Otel.defaultSpanArguments getCurrentTimeZone
    NewStdGen -> inSpan' "NewStdGen" Otel.defaultSpanArguments newStdGen
    NewUniqueHash -> inSpan' "NewUniqueHash" Otel.defaultSpanArguments newUniqueHash
    OpenURL u ->
        inSpan'
            "OpenURL"
            Otel.defaultSpanArguments{Otel.attributes = one ("url", Otel.toAttribute u)}
            $ openURL u
    ReadFileLazy f ->
        inSpan'
            "ReadFileLazy"
            Otel.defaultSpanArguments{Otel.attributes = one ("file", Otel.toAttribute $ toText f)}
            $ readFileLazy f
    ReadFileStrict f ->
        inSpan'
            "ReadFileStrict"
            Otel.defaultSpanArguments{Otel.attributes = one ("file", Otel.toAttribute $ toText f)}
            $ readFileStrict f
    ReadStdinStrict -> inSpan' "ReadStdinStrict" Otel.defaultSpanArguments readStdinStrict
    Glob g -> inSpan' "Glob" Otel.defaultSpanArguments $ glob g
    FileExists f ->
        inSpan'
            "FileExists"
            Otel.defaultSpanArguments{Otel.attributes = one ("file", Otel.toAttribute $ toText f)}
            $ fileExists f
    GetDataFileName f ->
        inSpan'
            "GetDataFileName"
            Otel.defaultSpanArguments{Otel.attributes = one ("file", Otel.toAttribute $ toText f)}
            $ getDataFileName f
    GetModificationTime f ->
        inSpan' "GetModificationTime" Otel.defaultSpanArguments $ getModificationTime f
    GetCommonState -> inSpan' "GetCommonState" Otel.defaultSpanArguments getCommonState
    GetsCommonState f -> inSpan' "GetsCommonState" Otel.defaultSpanArguments $ getsCommonState f
    PutCommonState s -> inSpan' "PutCommonState" Otel.defaultSpanArguments $ putCommonState s
    ModifyCommonState f ->
        inSpan' "ModifyCommonState" Otel.defaultSpanArguments $ modifyCommonState f
    LogOutput msg -> inSpan' "LogOutput" Otel.defaultSpanArguments $ logOutput msg
    Trace t -> inSpan' "Trace" Otel.defaultSpanArguments $ trace t

instance Member (Error P.PandocError) r => CE.MonadError P.PandocError (Sem r) where
    throwError = throw @P.PandocError
    catchError = catch @P.PandocError

instance Members '[PandocP, Error P.PandocError] r => P.PandocMonad (Sem r) where
    lookupEnv = lookupEnv
    getCurrentTime = getCurrentTime
    getCurrentTimeZone = getCurrentTimeZone
    newStdGen = newStdGen
    newUniqueHash = newUniqueHash
    openURL = openURL
    readFileLazy = readFileLazy
    readFileStrict = readFileStrict
    readStdinStrict = readStdinStrict
    glob = glob
    fileExists = fileExists
    getDataFileName = getDataFileName
    getModificationTime = getModificationTime
    getCommonState = getCommonState
    getsCommonState = getsCommonState
    putCommonState = putCommonState
    modifyCommonState = modifyCommonState
    logOutput = logOutput
    trace = trace

data Render m a where
    RenderMarkdown :: Text -> Render m Html

makeSem ''Render

runRenderViaPandoc ::
    (Members [PandocP, Error P.PandocError, Tracing] r) =>
    P.ReaderOptions ->
    Sem (Render : r) a ->
    Sem r a
runRenderViaPandoc opts = interpret \case
    RenderMarkdown i -> do
        x <- inSpan' "readMarkdonw" Otel.defaultSpanArguments $ P.readMarkdown opts i
        inSpan' "writeHtml5" Otel.defaultSpanArguments $
            P.writeHtml5 P.def{P.writerHighlightStyle = Just P.zenburn} x

renderWithCache ::
    Members [Render, Embed IO, KVStore Text Html] r =>
    Sem r a ->
    Sem r a
renderWithCache = intercept @Render \case
    RenderMarkdown i -> do
        mres <- lookupKV i
        case mres of
            Nothing -> do
                res <- renderMarkdown i
                writeKV i res
                pure res
            Just res -> pure res

traceRender :: Members [Tracing, Render] r => Sem r a -> Sem r a
traceRender = intercept @Render \case
    RenderMarkdown i -> inSpan' "render-markdown" Otel.defaultSpanArguments $ renderMarkdown i
