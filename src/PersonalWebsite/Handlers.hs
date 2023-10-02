module PersonalWebsite.Handlers (API, server, api) where

import Network.HTTP.Types hiding (Header)
import Network.Wai (responseLBS)
import PersonalWebsite.API
import PersonalWebsite.Blogs
import PersonalWebsite.Colors
import PersonalWebsite.Cookies
import PersonalWebsite.Home
import PersonalWebsite.Pages
import PersonalWebsite.Pandoc
import PersonalWebsite.Toys
import Polysemy
import Polysemy.Input
import Polysemy.Reader
import Relude hiding (MonadReader, Reader, ask, local, runReader)
import Servant
import Text.Blaze.Renderer.Utf8

server ::
    Members [Blogs, Render, Input Tags, Embed IO] r =>
    ServerT API (Sem r)
server sess =
    hoistServer (Proxy @APIWithoutPalette) (runReader seed') $
        homeHandler
            :<|> blogsHandler
            :<|> toysHandler
            :<|> paletteHandler
            :<|> pure notFoundHandler
  where
    seed' = coerce sess ?: ColorSeed 86
    notFoundHandler _ res =
        renderSite None lostPage
            & runReader seed'
            & run
            & renderMarkup
            & responseLBS status404 [("Content-Type", "text/html; charset=UTF-8")]
            & res
