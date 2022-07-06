module PersonalWebsite.API (API, server, api) where

import Capability.Reader
import Network.HTTP.Types hiding (Header)
import Network.Wai (responseLBS)
import PersonalWebsite.Blogs
import PersonalWebsite.Colors
import PersonalWebsite.Cookies
import PersonalWebsite.Home
import PersonalWebsite.Pages
import PersonalWebsite.Toys
import Relude hiding (MonadReader, ask, local)
import Servant
import Text.Blaze.Html
import Text.Blaze.Renderer.Utf8
import Text.Pandoc

notFoundHandler :: Int -> ServerT Raw m
notFoundHandler seed' = do
    pure $ \_ res ->
        res . responseLBS status404 [("Content-Type", "text/html; charset=UTF-8")]
            . renderMarkup
            $ runPurely lostPage
  where
    runPurely :: MonadReader (ReaderT Int Identity) Html -> Html
    runPurely a = runIdentity $ runReaderT (coerce a) seed'

type APIWithoutPalette =
    HomeAPI
        :<|> BlogsAPI
        :<|> ToysAPI
        :<|> PaletteAPI
        :<|> Raw

type API = Header "Cookie" SessionData :> APIWithoutPalette

server ::
    ( PandocMonad m
    , HasBlogRepo m
    , HasTags m
    , HasReader "colorSeed" Int m
    , MonadIO m
    ) =>
    ServerT API m
server sess =
    hoistServer
        (Proxy @APIWithoutPalette)
        (local @"colorSeed" $ const seed')
        $ homeHandler
            :<|> blogsHandler
            :<|> toysHandler
            :<|> paletteHandler
            :<|> notFoundHandler seed'
  where
    seed' = fromMaybe 0 $ coerce sess

api :: Proxy API
api = Proxy @API
