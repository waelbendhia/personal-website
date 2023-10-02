{-# OPTIONS_GHC -Wno-orphans #-}

module PersonalWebsite.Katip (Katiper, runKatipContext) where

import qualified Katip as K
import Polysemy
import Relude hiding (Reader, ask, local)

data Katiper m a where
    GetLogEnv :: Katiper m K.LogEnv
    LocalLogEnv :: (K.LogEnv -> K.LogEnv) -> m a -> Katiper m a
    GetKatipContext :: Katiper m K.LogContexts
    LocalKatipContext :: (K.LogContexts -> K.LogContexts) -> m a -> Katiper m a
    GetKatipNamespace :: Katiper m K.Namespace
    LocalKatipNamespace :: (K.Namespace -> K.Namespace) -> m a -> Katiper m a

makeSem ''Katiper

instance Members '[Katiper, Embed IO] r => K.Katip (Sem r) where
    getLogEnv = getLogEnv
    localLogEnv = localLogEnv

instance Members '[Katiper, Embed IO] r => K.KatipContext (Sem r) where
    getKatipContext = getKatipContext
    localKatipContext = localKatipContext
    getKatipNamespace = getKatipNamespace
    localKatipNamespace = localKatipNamespace

runKatipContext ::
    K.LogEnv ->
    K.LogContexts ->
    K.Namespace ->
    Sem (Katiper : r) a ->
    Sem r a
runKatipContext le lc ns = interpretH \case
    GetLogEnv -> pureT le
    LocalLogEnv f a -> do
        a' <- runT a
        raise $ runKatipContext (f le) lc ns a'
    GetKatipContext -> pureT lc
    LocalKatipContext f a -> do
        a' <- runT a
        raise $ runKatipContext le (f lc) ns a'
    GetKatipNamespace -> pureT ns
    LocalKatipNamespace f a -> do
        a' <- runT a
        raise $ runKatipContext le lc (f ns) a'
