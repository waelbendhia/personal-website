{-# LANGUAGE TemplateHaskell #-}

module Application (runApp, configParser) where

import Control.Concurrent
import Control.Concurrent.Async
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Data.Cache as C
import Data.Time
import Katip hiding (getEnvironment)
import qualified Katip.Wai as KW
import Network.Wai.Handler.Warp as Warp
import OpenTelemetry.Instrumentation.Wai
import qualified OpenTelemetry.Trace as T
import Options.Applicative
import PersonalWebsite.API
import PersonalWebsite.About
import PersonalWebsite.Blogs
import PersonalWebsite.Handlers
import PersonalWebsite.KVCache
import PersonalWebsite.Katip as PK
import PersonalWebsite.LiveReload
import PersonalWebsite.Monad
import PersonalWebsite.Pandoc
import PersonalWebsite.Tracing
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Resource
import Polysemy.State
import Relude hiding (Reader, ask, evalState, runReader)
import Servant
import System.Environment hiding (lookupEnv)
import System.Posix as Posix hiding (Resource, getEnvironment)
import System.Random
import Text.Blaze.Html
import Text.Pandoc hiding (lookupEnv)
import qualified Text.Pandoc as P hiding (lookupEnv)

data ApplicationConfig = ApplicationConfig
    { port :: !Int
    , staticAssets :: !Text
    , publicFolder :: !Text
    }
    deriving (Show)

staticAssetsParser :: Parser Text
staticAssetsParser =
    strOption
        ( long "static-folder"
            <> short 's'
            <> help "folder containing static assets"
            <> showDefault
        )

publicAssetsParser :: Parser Text
publicAssetsParser =
    strOption
        ( long "public-folder"
            <> short 'f'
            <> help "folder containing public assets"
            <> showDefault
        )

configParser :: Parser ApplicationConfig
configParser =
    ApplicationConfig
        <$> option
            auto
            ( long "port"
                <> short 'p'
                <> help "port to listen on"
                <> value 8081
                <> showDefault
            )
        <*> staticAssetsParser
        <*> publicAssetsParser

data AppContext = AppContext
    { renderCache :: !(C.Cache ByteString Html)
    , blogCache :: !(C.Cache ByteString (UTCTime, BlogEntry))
    , stateIORef :: !(IORef CommonState)
    , refreshChan :: !(Maybe (Chan ShouldRefresh))
    , env :: !(Map Text Text)
    , staticFolder :: !Text
    , publicFolder :: !Text
    }

mkApplication ::
    AppContext ->
    KW.ApplicationT (Sem [Katiper, Tracing, Resource, Embed IO])
mkApplication
    ( AppContext
            { renderCache = renderCache'
            , blogCache = blogCache'
            , stateIORef = stateIORef'
            , env = env'
            , staticFolder = static'
            , publicFolder = public'
            , refreshChan = refreshCh
            }
        ) =
        KW.middleware DebugS \req send' -> do
            le <- getLogEnv
            lc <- getKatipContext
            ns <- getKatipNamespace
            tp <- getTracerProvider
            let getRefreshSignal =
                    join <$> forM refreshCh \ch -> do
                        refreshSig <-
                            race
                                (threadDelay (pollingInterval * 1000000))
                                (readChan ch)
                        pure $ either (const Nothing) Just refreshSig
                hoistedApp =
                    serve api
                        $ hoistServer
                            api
                            ( Handler
                                . ExceptT
                                . runM
                                . runInputConst
                                    (UseLiveReload $ isJust refreshCh)
                                . runInputSem
                                    (embed @IO getRefreshSignal)
                                . runInputSem
                                    (embed @IO $ randomRIO @Int (minInt, maxInt))
                                . runResource
                                . runTracing tp
                                . runError
                                . mapError @PandocError (toServerError . AppPandoc)
                                . mapError @CVError (toServerError . AppCV)
                                . runStateIORef @CommonState stateIORef'
                                . runKatipContext le lc ns
                                . runPandocIO env'
                                . tracePandoc
                                . runKVWithCache @Text @(UTCTime, BlogEntry)
                                    (SHA512.hash . encodeUtf8)
                                    blogCache'
                                    1024
                                . runInputCV static'
                                . runBlogsFromFolder static'
                                . traceBlogs
                                . runKVWithCache @Text @Html
                                    (SHA512.hash . encodeUtf8)
                                    renderCache'
                                    1024
                                . runRenderViaPandoc
                                    P.def
                                        { P.readerExtensions =
                                            P.extensionsFromList
                                                [ P.Ext_backtick_code_blocks
                                                , P.Ext_fenced_code_attributes
                                                , P.Ext_fenced_code_blocks
                                                , P.Ext_header_attributes
                                                ]
                                        }
                                . renderWithCache
                                . traceRender
                                . runTagsFromFolder static'
                            )
                            (server public')
            embed
                $ hoistedApp req
                $ runM
                . runResource
                . runTracing tp
                . runKatipContext le lc ns
                . send'

withTraceProvider :: (Members [Embed IO, Resource] r) => Sem (Tracing : r) a -> Sem r a
withTraceProvider a = do
    shouldOtlp <- isJust <$> lookupEnv "OTEL_EXPORTER_OTLP_ENDPOINT"
    if shouldOtlp
        then
            bracket
                (embed T.initializeGlobalTracerProvider)
                (embed @IO . T.shutdownTracerProvider)
                (`runTracing` a)
        else do
            tp <- T.createTracerProvider [] T.emptyTracerProviderOptions
            runTracing tp a

runApp :: ApplicationConfig -> IO ()
runApp (ApplicationConfig p static' public') = void do
    env' <- fromList . fmap (bimap toText toText) <$> getEnvironment
    runM . runResource . withLogEnv $ withTraceProvider do
        le <- getLogEnv
        lc <- getKatipContext
        ns <- getKatipNamespace
        tp <- getTracerProvider
        otelMiddleware <- embed newOpenTelemetryWaiMiddleware
        let isInteractive = (== "<interactive>") <$> getProgName
        ctx <-
            embed
                ( AppContext
                    <$> C.newCache (Just $ fromInteger expiration)
                    <*> C.newCache (Just $ fromInteger expiration)
                    <*> newIORef def
                    <*> ifM
                        isInteractive
                        do
                            ch <- newChan
                            Just ch <$ writeChan ch ShouldRefresh
                        (pure Nothing)
                )
        let app =
                KW.runApplication
                    ( runM
                        . runResource
                        . runTracing tp
                        . runKatipContext le lc ns
                    )
                    (mkApplication $ ctx env' static' public')
            closeHandler closeSocket = void do
                let closeServer = Posix.Catch do
                        closeSocket
                        T.shutdownTracerProvider tp
                _ <- installHandler sigINT closeServer Nothing
                installHandler sigTERM closeServer Nothing
            warpSettings = Warp.defaultSettings & setPort p
        warpSettings' <-
            ifM
                (embed isInteractive)
                (pure $ warpSettings & setInstallShutdownHandler closeHandler)
                (pure warpSettings)
        hash <- Relude.lookupEnv "GIT_HASH"
        katipAddContext (sl "version" (fromMaybe "development" hash)) do
            $logTM InfoS "application starting"
            embed $ Warp.runSettings warpSettings' $ otelMiddleware app
  where
    expiration = 60 * 60 * ((10 :: Integer) ^ (9 :: Integer))
