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
import PersonalWebsite.Capabilities
import PersonalWebsite.Colors
import PersonalWebsite.Strategies
import Relude hiding (MonadReader, ask, local)
import Servant.Server
import Text.Pandoc as P
import UnliftIO hiding (catch)

data AppContext = AppContext
    { logEnv :: !LogEnv
    , logContexts :: !LogContexts
    , logNamespace :: !Namespace
    , colorMode :: !ColorMode
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
        (HasSource "colorMode" ColorMode, HasReader "colorMode" ColorMode)
        via Field "colorMode" () AppMonad
    deriving (HasBlogRepo, HasSource "tags" [Text]) via (BlogRepoFromFolder AppMonad)
    deriving
        ( HasThrow "app" AppError
        , HasCatch "app" AppError
        )
        via MonadError (ReaderT AppContext (ExceptT AppError IO))
    deriving
        ( HasThrow "pandoc" PandocError
        , HasCatch "pandoc" PandocError
        )
        via Rename "AppPandoc" (Ctor "AppPandoc" "app" AppMonad)
    deriving (MTL.MonadError PandocError, PandocMonad) via PandocViaIO AppMonad

instance HasSource "folder" Text AppMonad where
    await_ _ = pure "/home/wael/Development/personal/personal-website/test-blogs"

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

withAppMonad :: AppMonad b -> IO (Either AppError b)
withAppMonad m = withLogEnv $ \le ->
    runAppMonad
        m
        AppContext
            { logEnv = le
            , logContexts = mempty
            , logNamespace = "PersonalWebsite"
            , colorMode = Dark
            }

instance Katip AppMonad where
    getLogEnv = ask @"logEnv"
    localLogEnv = local @"logEnv"

instance KatipContext AppMonad where
    getKatipContext = ask @"logContexts"
    localKatipContext = local @"logContexts"
    getKatipNamespace = ask @"logNamespace"
    localKatipNamespace = local @"logNamespace"
