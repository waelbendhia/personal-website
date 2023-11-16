{-# LANGUAGE TemplateHaskell #-}

module Application (runApp, configParser) where

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
import PersonalWebsite.Blogs
import PersonalWebsite.Handlers
import PersonalWebsite.KVCache
import PersonalWebsite.Katip as PK
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
import System.Environment
import System.Random
import Text.Blaze.Html
import Text.Pandoc
import qualified Text.Pandoc as P

data ApplicationConfig = ApplicationConfig {port :: !Int, source :: !Text}
    deriving (Show)

sourceParser :: Parser Text
sourceParser =
    strOption (long "folder" <> short 's' <> help "folder containing blog posts" <> showDefault)

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
        <*> sourceParser

mkApplication ::
    C.Cache ByteString Html ->
    C.Cache ByteString (UTCTime, BlogEntry) ->
    IORef CommonState ->
    Map Text Text ->
    Text ->
    KW.ApplicationT (Sem [Katiper, Tracing, Resource, Embed IO])
mkApplication renderCache blogCache stateIORef env src = KW.middleware DebugS \req send' -> do
    le <- getLogEnv
    lc <- getKatipContext
    ns <- getKatipNamespace
    tp <- getTracerProvider
    let hoistedApp =
            serve api
                $ hoistServer
                    api
                    ( Handler
                        . ExceptT
                        . runM
                        . runInputSem
                            (embed @IO $ randomRIO @Int (minInt, maxInt))
                        . runResource
                        . runTracing tp
                        . runError
                        . mapError @PandocError (toServerError . AppPandoc)
                        . runStateIORef @CommonState stateIORef
                        . runKatipContext le lc ns
                        . runPandocIO env
                        . tracePandoc
                        . runKVWithCache @Text @(UTCTime, BlogEntry)
                            (SHA512.hash . encodeUtf8)
                            blogCache
                            1024
                        . runBlogsFromFolder src
                        . traceBlogs
                        . runKVWithCache @Text @Html
                            (SHA512.hash . encodeUtf8)
                            renderCache
                            1024
                        . runRenderViaPandoc
                            P.def
                                { P.readerExtensions =
                                    P.extensionsFromList
                                        [ P.Ext_backtick_code_blocks
                                        , P.Ext_fenced_code_attributes
                                        , P.Ext_fenced_code_blocks
                                        ]
                                }
                        . renderWithCache
                        . traceRender
                        . runTagsFromFolder src
                    )
                    server
    embed $ hoistedApp req (runM . runResource . runTracing tp . runKatipContext le lc ns . send')

withTraceProvider :: (Members [Embed IO, Resource] r) => Sem (Tracing : r) a -> Sem r a
withTraceProvider a =
    bracket
        (embed T.initializeGlobalTracerProvider)
        (embed @IO . T.shutdownTracerProvider)
        (`runTracing` a)

runApp :: ApplicationConfig -> IO ()
runApp (ApplicationConfig p src) = void do
    env <- fromList . fmap (bimap toText toText) <$> getEnvironment
    runM . runResource . withLogEnv $ withTraceProvider do
        le <- getLogEnv
        lc <- getKatipContext
        ns <- getKatipNamespace
        tp <- getTracerProvider
        otelMiddleware <- embed newOpenTelemetryWaiMiddleware
        renderCache <- embed $ C.newCache (Just $ fromInteger expiration)
        blogCache <- embed $ C.newCache (Just $ fromInteger expiration)
        stateIORef <- newIORef def
        let app =
                KW.runApplication
                    (runM . runResource . runTracing tp . runKatipContext le lc ns)
                    (mkApplication renderCache blogCache stateIORef env src)
        hash <- Relude.lookupEnv "GIT_HASH"
        katipAddContext
            (sl "version" (fromMaybe "development" hash))
            do
                $logTM InfoS "application starting"
                embed $ Warp.run p $ otelMiddleware app
  where
    expiration = 60 * 60 * ((10 :: Integer) ^ (9 :: Integer))
