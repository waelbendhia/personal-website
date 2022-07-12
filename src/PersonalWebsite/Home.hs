module PersonalWebsite.Home (
    module PersonalWebsite.Home.Pages,
    homeHandler,
) where

import Capability.Reader
import PersonalWebsite.Blogs
import PersonalWebsite.Home.API
import PersonalWebsite.Home.Pages
import PersonalWebsite.Pages
import Relude hiding (MonadReader, ask, local)
import Servant
import Text.Pandoc

homeHandler ::
    (PandocMonad m, HasBlogs m, HasReader "colorSeed" Int m) =>
    ServerT HomeAPI m
homeHandler = do
    bs <- getBlogs 0 Nothing
    renderSite Home =<< homePage (viaNonEmpty head bs)
