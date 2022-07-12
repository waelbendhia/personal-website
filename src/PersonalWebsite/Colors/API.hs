module PersonalWebsite.Colors.API (
    PaletteAPI,
    SetPaletteHeaders,
    SetSeedAPI,
    RandomizeAPI,
    Seed (..),
) where

import Relude
import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html
import Web.Cookie
import Web.FormUrlEncoded

type SetPaletteHeaders v =
    Headers '[Header "Set-Cookie" SetCookie, Header "Location" Text] v

newtype Seed = Seed Int

instance FromForm Seed where
    fromForm f = Seed <$> parseUnique "seed" f

type SetSeedAPI =
    "set-seed"
        :> Header "referer" Text
        :> ReqBody '[FormUrlEncoded] Seed
        :> Verb 'POST 303 '[HTML] (SetPaletteHeaders Html)

type RandomizeAPI =
    "randomize"
        :> Header "referer" Text
        :> Verb 'POST 303 '[HTML] (SetPaletteHeaders Html)

type PaletteAPI = SetSeedAPI :<|> RandomizeAPI
