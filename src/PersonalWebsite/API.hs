module PersonalWebsite.API (
    API,
    APIWithoutPalette,
    FaviconAPI,
    api,
    apiLink,
    StreamojiAPI,
    Event (..),
) where

import PersonalWebsite.About.API
import PersonalWebsite.Blogs.API
import PersonalWebsite.Colors.API
import PersonalWebsite.Cookies
import PersonalWebsite.HTMX
import PersonalWebsite.Home.API
import PersonalWebsite.Image
import PersonalWebsite.LiveReload
import PersonalWebsite.Toys.API
import Relude hiding (MonadReader, ask, local)
import Servant

data EventStream deriving (Typeable)

instance Accept EventStream where
    contentType _ = "text/event-stream"

data Event = Event !Text !Text

instance MimeRender EventStream Event where
    mimeRender _ (Event e d) = encodeUtf8 $ "event: " <> e <> "\ndata: " <> d <> "\n\n"

type FaviconAPI =
    "favicon.ico" :> Get '[ICO] (Headers '[Header "Cache-Control" Text] LByteString)

type StreamojiAPI = "how-was-i-made" :> StreamGet NoFraming EventStream (SourceIO Event)

type APIWithoutPalette =
    HomeAPI
        :<|> AboutAPI
        :<|> BlogsAPI
        :<|> ToysAPI
        :<|> PaletteAPI
        :<|> FaviconAPI
        :<|> LiveReloadAPI
        :<|> "public" :> Raw
        :<|> StreamojiAPI
        :<|> Raw

type API = Header "Cookie" SessionData :> Header "Hx-Request" IsHXRequest :> APIWithoutPalette

api :: Proxy API
api = Proxy @API

apiLink :: (IsElem endpoint API, HasLink endpoint) => Proxy endpoint -> MkLink endpoint Link
apiLink = safeLink (Proxy @API)
