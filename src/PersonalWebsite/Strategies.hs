{-# LANGUAGE TemplateHaskell #-}

module PersonalWebsite.Strategies (PandocViaIO (..)) where

import Capability.Error
import qualified Control.Monad.Except as MTL
import Katip
import Relude
import Text.Pandoc as P

newtype PandocViaIO m a = PandocViaIO (m a) deriving (Functor, Applicative, Monad)

throwPandoc :: (HasThrow "pandoc" PandocError m) => m (Either PandocError b) -> m b
throwPandoc = (either (throw @"pandoc") pure =<<)

instance
    ( Monad m
    , HasThrow "pandoc" PandocError m
    , HasCatch "pandoc" PandocError m
    ) =>
    MTL.MonadError PandocError (PandocViaIO m)
    where
    throwError = PandocViaIO . throw @"pandoc"
    catchError f m = PandocViaIO $ catch @"pandoc" (coerce f) (coerce m)

liftPandocViaIO :: (MonadIO m, HasThrow "pandoc" PandocError m) => PandocIO a -> PandocViaIO m a
liftPandocViaIO = PandocViaIO . throwPandoc . liftIO . runIO

instance
    {-# OVERLAPPING #-}
    ( HasThrow "pandoc" PandocError m
    , HasCatch "pandoc" PandocError m
    , MonadIO m
    , KatipContext m
    ) =>
    PandocMonad (PandocViaIO m)
    where
    lookupEnv = liftPandocViaIO . P.lookupEnv
    getCurrentTime = liftPandocViaIO getCurrentTime
    getCurrentTimeZone = liftPandocViaIO getCurrentTimeZone
    newStdGen = liftPandocViaIO newStdGen
    newUniqueHash = liftPandocViaIO newUniqueHash
    openURL = liftPandocViaIO . openURL
    readFileLazy = liftPandocViaIO . readFileLazy
    readFileStrict = liftPandocViaIO . readFileStrict
    readStdinStrict = liftPandocViaIO readStdinStrict
    glob = liftPandocViaIO . glob
    fileExists = liftPandocViaIO . fileExists
    getDataFileName = liftPandocViaIO . getDataFileName
    getModificationTime = liftPandocViaIO . getModificationTime
    getCommonState = liftPandocViaIO getCommonState
    putCommonState = liftPandocViaIO . putCommonState
    logOutput msg = PandocViaIO $ $(logTM) DebugS (show msg)

newtype PandocViaPure m a = PandocViaPure (m a)
    deriving (Functor, Applicative, Monad)

instance
    ( Monad m
    , HasThrow "pandoc" PandocError m
    , HasCatch "pandoc" PandocError m
    ) =>
    MTL.MonadError PandocError (PandocViaPure m)
    where
    throwError = PandocViaPure . throw @"pandoc"
    catchError m f = PandocViaPure $ catch @"pandoc" (coerce m) (coerce f)

liftPandocViaPure ::
    (HasThrow "pandoc" PandocError m) =>
    PandocPure a ->
    PandocViaPure m a
liftPandocViaPure m = PandocViaPure $ either (throw @"pandoc") pure $ runPure m

instance
    {-# OVERLAPPING #-}
    ( HasThrow "pandoc" PandocError m
    , HasCatch "pandoc" PandocError m
    , KatipContext m
    ) =>
    PandocMonad (PandocViaPure m)
    where
    lookupEnv = liftPandocViaPure . P.lookupEnv
    getCurrentTime = liftPandocViaPure getCurrentTime
    getCurrentTimeZone = liftPandocViaPure getCurrentTimeZone
    newStdGen = liftPandocViaPure newStdGen
    newUniqueHash = liftPandocViaPure newUniqueHash
    openURL = liftPandocViaPure . openURL
    readFileLazy = liftPandocViaPure . readFileLazy
    readFileStrict = liftPandocViaPure . readFileStrict
    readStdinStrict = liftPandocViaPure readStdinStrict
    glob = liftPandocViaPure . glob
    fileExists = liftPandocViaPure . fileExists
    getDataFileName = liftPandocViaPure . getDataFileName
    getModificationTime = liftPandocViaPure . getModificationTime
    getCommonState = liftPandocViaPure getCommonState
    putCommonState = liftPandocViaPure . putCommonState
    logOutput msg = PandocViaPure $ $(logTM) DebugS (show msg)
