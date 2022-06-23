module PersonalWebsite.Monad (AppMonad (..),
  AppContext (..), withAppMonad) where

import Capability.Error hiding (MonadUnliftIO)
import qualified Capability.Error as CE
import Capability.Reader
import Capability.Source
import qualified Control.Monad.Except as MTL
import Katip
import Relude hiding (MonadReader, ask, local)
import Text.Pandoc as P
import UnliftIO hiding (catch)
import PersonalWebsite.BlogRepo 

data AppContext = AppContext
    { logEnv :: !LogEnv
    , logContexts :: !LogContexts
    , logNamespace :: !Namespace
    }
    deriving (Generic)

newtype AppMonad a = AppMonad {runAppMonad :: AppContext -> IO a}
    deriving (Functor, Applicative, Monad, MonadIO) via ReaderT AppContext IO
    deriving
        (HasSource () AppContext, HasReader () AppContext)
        via MonadReader (ReaderT AppContext IO)
    deriving
        (HasSource "logEnv" LogEnv, HasReader "logEnv" LogEnv)
        via Field "logEnv" () AppMonad
    deriving
        (HasSource "logContexts" LogContexts, HasReader "logContexts" LogContexts)
        via Field "logContexts" () AppMonad
    deriving
        (HasSource "logNamespace" Namespace, HasReader "logNamespace" Namespace)
        via Field "logNamespace" () AppMonad
    deriving (HasBlogRepo) via (BlogRepoFromFolder AppMonad)
    deriving
        ( HasThrow "pandoc" PandocError
        , HasCatch "pandoc" PandocError
        )
        via CE.MonadUnliftIO PandocError AppMonad

instance MTL.MonadError PandocError AppMonad where
    throwError = throw @"pandoc"
    catchError = catch @"pandoc"

instance HasSource "folder" Text AppMonad where
  await_ _ = pure "/home/wael/Development/personal/personal-website/test-blogs"

throwPandoc :: AppMonad (Either PandocError b) -> AppMonad b
throwPandoc = (either (throw @"pandoc") pure =<<)

instance PandocMonad AppMonad where
    lookupEnv = throwPandoc . liftIO . runIO . P.lookupEnv
    getCurrentTime = throwPandoc . liftIO . runIO $ P.getCurrentTime
    getCurrentTimeZone = throwPandoc . liftIO . runIO $ P.getCurrentTimeZone
    newStdGen = throwPandoc . liftIO . runIO $ P.newStdGen
    newUniqueHash = throwPandoc . liftIO . runIO $ P.newUniqueHash
    openURL = throwPandoc . liftIO . runIO . P.openURL
    readFileLazy = throwPandoc . liftIO . runIO . P.readFileLazy
    readFileStrict = throwPandoc . liftIO . runIO . P.readFileStrict
    readStdinStrict = throwPandoc . liftIO . runIO $ P.readStdinStrict
    glob = throwPandoc . liftIO . runIO . P.glob
    fileExists = throwPandoc . liftIO . runIO . P.fileExists
    getDataFileName = throwPandoc . liftIO . runIO . P.getDataFileName
    getModificationTime = throwPandoc . liftIO . runIO . P.getModificationTime
    getCommonState = throwPandoc . liftIO . runIO $ P.getCommonState
    putCommonState = throwPandoc . liftIO . runIO . P.putCommonState
    logOutput msg = $(logTM) DebugS (show msg)

withLogEnv :: (LogEnv -> IO b) -> IO b
withLogEnv f = do
    handleScribe <-
        mkHandleScribeWithFormatter
            jsonFormat
            ColorIfTerminal
            stdout
            (permitItem DebugS)
            V2
    let makeLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "PersonalWebsite" "production"
    bracket makeLogEnv closeScribes f

withAppMonad :: AppMonad b -> IO b
withAppMonad m = withLogEnv $ \le ->
    runAppMonad m AppContext{logEnv = le, logContexts = mempty, logNamespace = "PersonalWebsite"}

instance Katip AppMonad where
    getLogEnv = ask @"logEnv"
    localLogEnv = local @"logEnv"

instance KatipContext AppMonad where
    getKatipContext = ask @"logContexts"
    localKatipContext = local @"logContexts"
    getKatipNamespace = ask @"logNamespace"
    localKatipNamespace = local @"logNamespace"

instance MonadUnliftIO AppMonad where
    withRunInIO f = AppMonad $ \ctx -> f (`runAppMonad` ctx)
