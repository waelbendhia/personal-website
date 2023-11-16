module PersonalWebsite.API (API, APIWithoutPalette, api, apiLink) where

import PersonalWebsite.About.API
import PersonalWebsite.Blogs.API
import PersonalWebsite.Colors.API
import PersonalWebsite.Cookies
import PersonalWebsite.Home.API
import PersonalWebsite.Toys.API
import Relude hiding (MonadReader, ask, local)
import Servant

type APIWithoutPalette =
    HomeAPI
        :<|> AboutAPI
        :<|> BlogsAPI
        :<|> ToysAPI
        :<|> PaletteAPI
        :<|> Raw

type API = Header "Cookie" SessionData :> APIWithoutPalette

api :: Proxy API
api = Proxy @API

apiLink :: (IsElem endpoint API, HasLink endpoint) => Proxy endpoint -> MkLink endpoint Link
apiLink = safeLink (Proxy @API)
