module PersonalWebsite.API (API, server, api) where

import Capability.Reader
import Network.HTTP.Types hiding (Header)
import Network.Wai (responseLBS)
import PersonalWebsite.API.Container
import PersonalWebsite.Capabilities
import PersonalWebsite.Colors
import PersonalWebsite.Cookies
import PersonalWebsite.Pages.Blog
import PersonalWebsite.Pages.Home
import Relude hiding (MonadReader, ask, local)
import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html
import Text.Blaze.Renderer.Utf8
import Text.Pandoc
import Web.Cookie

type HomeAPI = Get '[HTML] Html

homeHandler ::
    (PandocMonad m, HasBlogRepo m, HasReader "colorMode" ColorMode m) =>
    ServerT HomeAPI m
homeHandler = do
    bs <- getBlogs 0 Nothing
    renderSite Home =<< homePage (viaNonEmpty head bs)

type BlogsAPI = "blog" :> QueryParam "page" Int :> QueryParam "tag" Text :> Get '[HTML] Html

blogsHandler ::
    (PandocMonad m, HasBlogRepo m, HasReader "colorMode" ColorMode m) => ServerT BlogsAPI m
blogsHandler page tag = renderSite Blog =<< blogsPage (fromMaybe 0 page) tag

type TagsAPI = "blog" :> "tags" :> Get '[HTML] Html

tagsHandler ::
    (PandocMonad m, HasTags m, HasReader "colorMode" ColorMode m) =>
    ServerT TagsAPI m
tagsHandler = renderSite Blog =<< tagsPage

type ToysAPI = "toys" :> Get '[HTML] Html

toysHandler :: (HasReader "colorMode" ColorMode m) => ServerT ToysAPI m
toysHandler = renderSite Toys ("This is where the toys live" :: Text)

type BlogAPI = "blog" :> Capture "path" Text :> Get '[HTML] Html

lostPage :: (HasReader "colorMode" ColorMode m) => m Html
lostPage = renderSite None lost

blogHandler ::
    (PandocMonad m, HasBlogRepo m, HasReader "colorMode" ColorMode m) =>
    ServerT BlogAPI m
blogHandler =
    getBlog >=> maybe lostPage (renderBlogEntry >=> renderSite Blog)

notFoundHandler :: ColorMode -> ServerT Raw m
notFoundHandler mode' = do
    pure $ \_ res ->
        res . responseLBS status404 [("Content-Type", "text/html; charset=UTF-8")]
            . renderMarkup
            $ runPurely lostPage
  where
    runPurely :: MonadReader (ReaderT ColorMode Identity) Html -> Html
    runPurely a = runIdentity $ runReaderT (coerce a) mode'

type ToggleHeaders v =
    Headers '[Header "Set-Cookie" SetCookie, Header "Location" Text] v

type ToggleAPI =
    "toggle"
        :> Header "referer" Text
        :> Verb
            'POST
            303
            '[HTML]
            (ToggleHeaders Html)

toggleHeaders :: SetCookie -> Maybe Text -> v -> ToggleHeaders v
toggleHeaders c ref = addHeader c . addHeader (fromMaybe "" ref)

toggleHandler :: Monad m => ColorMode -> ServerT ToggleAPI m
toggleHandler mode' ref = pure $ toggleHeaders cookie ref ""
  where
    cookie =
        defaultSetCookie
            { setCookieName = "session-data"
            , setCookieValue = case mode' of
                Dark -> "light"
                Light -> "dark"
            }

type APIWithoutPalette =
    HomeAPI
        :<|> BlogsAPI
        :<|> TagsAPI
        :<|> BlogAPI
        :<|> ToysAPI
        :<|> ToggleAPI
        :<|> Raw

type API = Header "Cookie" SessionData :> APIWithoutPalette

server ::
    ( PandocMonad m
    , HasBlogRepo m
    , HasTags m
    , HasReader "colorMode" ColorMode m
    ) =>
    ServerT API m
server sess =
    hoistServer
        (Proxy @APIWithoutPalette)
        (local @"colorMode" $ const mode')
        $ homeHandler
            :<|> blogsHandler
            :<|> tagsHandler
            :<|> blogHandler
            :<|> toysHandler
            :<|> toggleHandler mode'
            :<|> notFoundHandler mode'
  where
    mode' = fromMaybe Dark $ coerce sess

api :: Proxy API
api = Proxy @API
