module PersonalWebsite.Blogs (
    module PersonalWebsite.Blogs.Data,
    module PersonalWebsite.Blogs.Pages,
    module PersonalWebsite.Blogs.Capabilities,
    blogsHandler,
) where

import Capability.Reader
import PersonalWebsite.Blogs.API
import PersonalWebsite.Blogs.Capabilities
import PersonalWebsite.Blogs.Data
import PersonalWebsite.Blogs.Pages
import PersonalWebsite.Pages
import Relude
import Servant
import Text.Pandoc

blogsHandler ::
    (PandocMonad m, HasBlogs m, HasTags m, HasReader "colorSeed" Int m) =>
    ServerT BlogsAPI m
blogsHandler = pageBlogsHandler :<|> tagsHandler :<|> blogHandler
  where
    pageBlogsHandler page = blogsPage (fromMaybe 0 page) >=> renderSite Blog
    tagsHandler = tagsPage >>= renderSite Blog
    blogHandler =
        getBlog >=> maybe (renderSite None lostPage) (renderBlogEntry >=> renderSite Blog)
