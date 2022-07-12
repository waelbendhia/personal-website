module PersonalWebsite.Cookies (SessionData (..)) where

import Relude
import Servant
import Web.Cookie

newtype SessionData = SessionData {seed :: Int}

parseFromText :: Text -> Either Text SessionData
parseFromText t =
    maybe
        (Left $ "invalid color mode " <> t)
        (Right . SessionData)
        (readMaybe $ toString t)

instance FromHttpApiData SessionData where
    parseHeader v = do
        (_, sData) <-
            parseCookies v
                & find ((== "session-data") . fst)
                & maybe (Left "'session-data' cookie not found") pure
        parseFromText $ decodeUtf8 sData
    parseQueryParam = parseFromText
