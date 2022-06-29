module PersonalWebsite.Cookies (SessionData (..)) where

import qualified Data.Text as T
import PersonalWebsite.Colors
import Relude
import Servant
import Web.Cookie

newtype SessionData = SessionData {mode :: ColorMode}

parseFromText :: Text -> Either Text SessionData
parseFromText t = case T.toLower t of
    "dark" -> pure $ SessionData Dark
    "light" -> pure $ SessionData Light
    x -> Left $ "invalid color mode " <> x

instance FromHttpApiData SessionData where
    parseHeader v = do
        let cookies = parseCookies v
        (_, sData) <-
            maybe
                (Left "'session-data' cookie not found")
                pure
                (find ((== "session-data") . fst) cookies)
        parseFromText $ decodeUtf8 sData
    parseQueryParam = parseFromText
