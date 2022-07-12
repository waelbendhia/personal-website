module PersonalWebsite.Logging where

import Capability.Reader
import GHC.TypeLits
import Katip
import Relude hiding (ask, local)

newtype KatipFromReader (tags :: [Symbol]) m a = KatipFromReader (m a)
    deriving
        (Functor, Applicative, Monad, MonadIO)

instance
    ( tags ~ (logEnvTag ': rest)
    , HasReader logEnvTag LogEnv m
    , MonadIO m
    ) =>
    Katip (KatipFromReader tags m)
    where
    getLogEnv = KatipFromReader $ ask @logEnvTag
    localLogEnv f (KatipFromReader a) = KatipFromReader $ local @logEnvTag f a

instance
    ( tags ~ (logEnvTag ': logContextsTag ': logNamespaceTag ': rest)
    , HasReader logEnvTag LogEnv m
    , HasReader logContextsTag LogContexts m
    , HasReader logNamespaceTag Namespace m
    , MonadIO m
    ) =>
    KatipContext (KatipFromReader tags m)
    where
    getKatipContext = KatipFromReader $ ask @logContextsTag
    localKatipContext f (KatipFromReader a) = KatipFromReader $ local @logContextsTag f a
    getKatipNamespace = KatipFromReader $ ask @logNamespaceTag
    localKatipNamespace f (KatipFromReader a) = KatipFromReader $ local @logNamespaceTag f a
