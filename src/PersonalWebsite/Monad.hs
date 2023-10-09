module PersonalWebsite.Monad (
    AppError (..),
    toServerError,
    withLogEnv,
) where

import Katip
import PersonalWebsite.Katip
import Polysemy
import Polysemy.Resource
import Relude hiding (MonadReader, ask, local)
import Servant.Server
import Text.Pandoc as P

newtype AppError = AppPandoc PandocError
    deriving (Show, Exception, Generic)

toServerError :: AppError -> ServerError
toServerError (AppPandoc pe) = err500{errReasonPhrase = show pe}

withLogEnv :: Members [Embed IO, Resource] r => Sem (Katiper : r) a -> Sem r a
withLogEnv a = do
    handleScribe <-
        embed $
            mkHandleScribeWithFormatter
                jsonFormat
                ColorIfTerminal
                stdout
                (permitItem DebugS)
                V2
    let makeLogEnv = embed do
            le <- initLogEnv "PersonalWebsite" "production"
            registerScribe "stdout" handleScribe defaultScribeSettings le
    bracket makeLogEnv (embed . closeScribes) \le -> runKatipContext le mempty "PersonalWebsite" a
