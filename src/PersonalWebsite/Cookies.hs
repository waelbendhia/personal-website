module PersonalWebsite.Cookies (SessionData (..)) where

import Relude
import Servant
import Web.Cookie

newtype SessionData = SessionData {seed :: Int}

parseFromText :: Text -> Either Text SessionData
parseFromText t =
    SessionData
        <$> maybe
            (Left $ "invalid color mode " <> t)
            Right
            (readMaybe (toString t))

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
