{-# LANGUAGE TemplateHaskell #-}

module Application (runApp) where

import Katip
import Katip.Wai (runApplication)
import qualified Katip.Wai as KW
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp
import PersonalWebsite.API
import PersonalWebsite.Monad
import Relude
import Servant
import UnliftIO

hoistApplication :: (forall a. m a -> n a) -> (forall a. n a -> m a) -> KW.ApplicationT m -> KW.ApplicationT n
hoistApplication hoist unhoist application request send =
    hoist $ application request (unhoist . send)

mkLoggerMiddleware :: (MonadUnliftIO m, KatipContext m) => m Middleware
mkLoggerMiddleware =
    withRunInIO $ \runInIO ->
        pure $ \app -> runApplication runInIO $ KW.middleware DebugS $ hoistApplication liftIO runInIO app

runApp :: IO ()
runApp =
    withAppMonad $ do
        loggerMiddleware <- mkLoggerMiddleware
        $logTM InfoS "application starting"
        withRunInIO $ \f ->
            run 8081
                . loggerMiddleware
                . serve api
                -- TODO: error handling
                $ hoistServer api (liftIO . f) server
