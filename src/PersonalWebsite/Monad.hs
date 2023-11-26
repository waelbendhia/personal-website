module PersonalWebsite.Monad (
    AppError (..),
    toServerError,
    withLogEnv,
) where

import Katip
import PersonalWebsite.About.Capabilities
import PersonalWebsite.Katip
import Polysemy
import Polysemy.Resource
import Relude hiding (MonadReader, ask, local)
import Servant.Server
import Text.Pandoc as P

data AppError = AppPandoc PandocError | AppCV CVError
    deriving (Show, Generic)

instance Exception AppError

toServerError :: AppError -> ServerError
toServerError (AppPandoc pe) = err500{errReasonPhrase = show pe}
toServerError (AppCV ce) = err500{errReasonPhrase = show ce}

withLogEnv :: (Members [Embed IO, Resource] r) => Sem (Katiper : r) a -> Sem r a
withLogEnv a = do
    handleScribe <-
        embed
            $ mkHandleScribeWithFormatter
                jsonFormat
                ColorIfTerminal
                stdout
                (permitItem DebugS)
                V2
    let makeLogEnv = embed do
            le <- initLogEnv "PersonalWebsite" "production"
            registerScribe "stdout" handleScribe defaultScribeSettings le
    bracket
        makeLogEnv
        (embed . closeScribes)
        \le -> runKatipContext le mempty "PersonalWebsite" a
