module PersonalWebsite.Cookies (SessionData (..)) where

import PersonalWebsite.Colors
import Relude
import Servant
import Web.Cookie

newtype SessionData = SessionData {seed :: ColorSeed}

parseFromText :: Text -> Maybe SessionData
parseFromText =
    fmap SessionData . fromString . toString

defaultSessionData :: SessionData
defaultSessionData = SessionData $ ColorSeed 86

instance FromHttpApiData SessionData where
    parseHeader v = Right $ fromMaybe defaultSessionData do
        (_, sData) <- find ((== "session-data") . fst) $ parseCookies v
        parseFromText $ decodeUtf8 sData
    parseQueryParam = Right . fromMaybe defaultSessionData . parseFromText
