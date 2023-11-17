module PersonalWebsite.About.Capabilities (
    runInputCV,
    CVError (..),
) where

import qualified Control.Exception as CE
import Data.Aeson
import Data.Yaml
import PersonalWebsite.About.Pages
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Relude

data CVError
    = FileReadError CE.IOException
    | ParseError ParseException
    deriving (Show)

instance Exception CVError

runInputCV ::
    forall r a.
    (Members '[Embed IO, Error CVError] r) =>
    Text ->
    Sem (Input ParsedCV : r) a ->
    Sem r a
runInputCV staticFolder = runInputSem do
    content <-
        fromExceptionVia FileReadError
            $ readFileBS (toString staticFolder <> "/cv.yaml")
    fromEither $ bimap ParseError ParsedCV $ decodeEither' @Value content
