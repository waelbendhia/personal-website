module PersonalWebsite.Monad (
    AppMonad (..),
    AppContext (..),
    withAppMonad,
    toServerError,
) where

import Capability.Error hiding (MonadUnliftIO)
import Capability.Reader
import Capability.Source
import qualified Control.Monad.Except as MTL
import Katip
import PersonalWebsite.Blogs
import PersonalWebsite.Logging
import PersonalWebsite.Strategies
import Relude hiding (MonadReader, ask, local)
import Servant.Server
import Text.Pandoc as P
import UnliftIO hiding (catch)

data AppContext = AppContext
    { logEnv :: !LogEnv
    , logContexts :: !LogContexts
    , logNamespace :: !Namespace
    , colorSeed :: !Int
    , blogSource :: !BlogSource
    }
    deriving (Generic)

newtype AppError = AppPandoc PandocError
    deriving (Show, Exception, Generic)

toServerError :: AppError -> ServerError
toServerError (AppPandoc pe) = err500{errReasonPhrase = show pe}

newtype AppMonad a = AppMonad {runAppMonad :: AppContext -> IO (Either AppError a)}
    deriving
        (Functor, Applicative, Monad, MonadIO)
        via ReaderT AppContext (ExceptT AppError IO)
    deriving
        (HasSource () AppContext, HasReader () AppContext)
        via MonadReader (ReaderT AppContext (ExceptT AppError IO))
    deriving
        (HasSource "logEnv" LogEnv, HasReader "logEnv" LogEnv)
        via Field "logEnv" () AppMonad
    deriving
        (HasSource "logContexts" LogContexts, HasReader "logContexts" LogContexts)
        via Field "logContexts" () AppMonad
    deriving
        (HasSource "logNamespace" Namespace, HasReader "logNamespace" Namespace)
        via Field "logNamespace" () AppMonad
    deriving
        (HasSource "colorSeed" Int, HasReader "colorSeed" Int)
        via Field "colorSeed" () AppMonad
    deriving
        (HasSource "blogSource" BlogSource, HasReader "blogSource" BlogSource)
        via Field "blogSource" () AppMonad
    deriving
        (Katip, KatipContext)
        via (KatipFromReader '["logEnv", "logContexts", "logNamespace"] AppMonad)
    deriving (HasBlogs, HasSource "tags" [Text]) via (BlogsFromSource AppMonad)
    deriving
        (HasThrow "app" AppError, HasCatch "app" AppError)
        via MonadError (ReaderT AppContext (ExceptT AppError IO))
    deriving
        (HasThrow "pandoc" PandocError, HasCatch "pandoc" PandocError)
        via Rename "AppPandoc" (Ctor "AppPandoc" "app" AppMonad)
    deriving (MTL.MonadError PandocError, PandocMonad) via PandocViaIO AppMonad

withLogEnv :: (LogEnv -> IO b) -> IO b
withLogEnv f = do
    handleScribe <-
        mkHandleScribeWithFormatter
            jsonFormat
            ColorIfTerminal
            stdout
            (permitItem DebugS)
            V2
    let makeLogEnv =
            registerScribe "stdout" handleScribe defaultScribeSettings
                =<< initLogEnv "PersonalWebsite" "production"
    bracket makeLogEnv closeScribes f

withAppMonad :: BlogSource -> AppMonad b -> IO (Either AppError b)
withAppMonad blogSource' m = withLogEnv $ \le ->
    runAppMonad
        m
        AppContext
            { logEnv = le
            , logContexts = mempty
            , logNamespace = "PersonalWebsite"
            , colorSeed = 0
            , blogSource = blogSource'
            }
