module PersonalWebsite.Home (
    module PersonalWebsite.Home.Pages,
    homeHandler,
) where

import PersonalWebsite.Blogs
import PersonalWebsite.Colors
import PersonalWebsite.Home.API
import PersonalWebsite.Home.Pages
import PersonalWebsite.Pages
import PersonalWebsite.Pandoc
import Polysemy
import Polysemy.Reader
import Relude hiding (MonadReader, Reader, ask, local)
import Servant
import Polysemy.Input

homeHandler ::
    (Members '[Blogs, Render, Reader ColorSeed, Input Int] r) =>
    ServerT HomeAPI (Sem r)
homeHandler = renderSite Home =<< homePage . viaNonEmpty head =<< getBlogs 0 Nothing
