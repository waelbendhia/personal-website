{-# LANGUAGE TemplateHaskell #-}

module Application (runApp) where

import Capability.Reader
import Katip
import Katip.Wai (runApplication)
import qualified Katip.Wai as KW
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp
import Options.Applicative
import PersonalWebsite.API
import PersonalWebsite.Monad
import Relude hiding (ask)
import Servant
import UnliftIO

data ApplicationConfig = ApplicationConfig
    { port :: !Int
    , source :: !Text
    }
    deriving (Show)

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
        <*> strOption
            ( long "source"
                <> short 's'
                <> help "source of glob posts"
                <> value ""
                <> showDefault
            )

hoistApplication ::
    (forall a. m a -> n a) ->
    (forall a. n a -> m a) ->
    KW.ApplicationT m ->
    KW.ApplicationT n
hoistApplication hoist unhoist application request send =
    hoist $ application request (unhoist . send)

mkLoggerMiddleware :: AppMonad Middleware
mkLoggerMiddleware = do
    ctx <- ask @()
    let runInIO :: forall a. AppMonad a -> IO a
        runInIO m = do
            x <- runAppMonad m ctx
            either throwIO pure x
    pure $ \app ->
        runApplication runInIO
            . KW.middleware DebugS
            $ hoistApplication liftIO runInIO app

toHandler :: AppContext -> AppMonad a -> Servant.Handler a
toHandler ctx = coerce . fmap (first toServerError) . (`runAppMonad` ctx)

runApp :: IO ()
runApp = void . withAppMonad $ do
    ApplicationConfig p _ <- liftIO $ execParser $ info configParser fullDesc
    loggerMiddleware <- mkLoggerMiddleware
    $logTM InfoS "application starting"
    ctx <- ask @()
    liftIO $
        run p
            . loggerMiddleware
            . serve api
            $ hoistServer api (toHandler ctx) server
