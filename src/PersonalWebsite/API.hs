module PersonalWebsite.API (API, server, api) where

import Network.HTTP.Types
import Network.Wai (responseLBS)
import PersonalWebsite.API.Container
import PersonalWebsite.BlogRepo
import PersonalWebsite.Markdown
import Relude
import Servant
import Servant.HTML.Blaze
import Text.Blaze
import Text.Blaze.Html
import Text.Blaze.Renderer.Utf8
import Text.Pandoc

type HomeAPI = Get '[HTML] (Container Text)

homeHandler :: Monad m => ServerT HomeAPI m
homeHandler = pure $ Container Home "you are home"

type BlogsAPI =
    "blog"
        :> QueryParam "page" Int
        :> Get '[HTML] (Container Html)

blogsHandler :: (PandocMonad m, HasBlogRepo m) => ServerT BlogsAPI m
blogsHandler page = do
    bs <- getBlogs (fromMaybe 0 page)
    p <- renderPage (fromMaybe 0 page) bs
    pure $ Container Blog p

type BlogAPI =
    "blog"
        :> Capture "path" Text
        :> Get '[HTML] (Container Html)

lostPage :: Container Html
lostPage = Container None "you seem lost"

blogHandler :: (PandocMonad m, HasBlogRepo m) => ServerT BlogAPI m
blogHandler path' = do
    b <- getBlog path'
    p <- mapM renderBlogEntry b
    pure $ maybe lostPage (Container Blog) p

notFoundHandler :: ServerT Raw m
notFoundHandler = pure $ \_ res ->
    res . responseLBS status404 [("Content-Type", "text/html; charset=UTF-8")]
        . renderMarkup
        . toMarkup
        $ lostPage

type API = HomeAPI :<|> BlogsAPI :<|> BlogAPI :<|> Raw

server :: (PandocMonad m, HasBlogRepo m) => ServerT API m
server = homeHandler :<|> blogsHandler :<|> blogHandler :<|> notFoundHandler

api :: Proxy API
api = Proxy @API
