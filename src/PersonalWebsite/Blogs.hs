module PersonalWebsite.Blogs (
    module PersonalWebsite.Blogs.Data,
    module PersonalWebsite.Blogs.Pages,
    module PersonalWebsite.Blogs.Capabilities,
    blogsHandler,
) where

import PersonalWebsite.Blogs.API
import PersonalWebsite.Blogs.Capabilities
import PersonalWebsite.Blogs.Data
import PersonalWebsite.Blogs.Pages
import PersonalWebsite.Colors
import PersonalWebsite.LiveReload
import PersonalWebsite.Pages
import PersonalWebsite.Pandoc
import Polysemy
import Polysemy.Input
import Polysemy.Reader
import Relude hiding (Reader)
import Servant

blogsHandler ::
    ( Members
        '[ Blogs
         , Render
         , Input Tags
         , Reader ColorSeed
         , Input Int
         , Input IsHXRequest
         , Input UseLiveReload
         ]
        r
    ) =>
    ServerT BlogsAPI (Sem r)
blogsHandler = pageBlogsHandler :<|> tagsHandler :<|> blogHandler
  where
    pageBlogsHandler page = renderSite Blog <=< blogsPage (fromMaybe 0 page)
    tagsHandler = renderSite Blog =<< tagsPage
    blogHandler =
        maybe (renderSite None lostPage) (renderSite Blog <=< renderBlogEntry True) <=< getBlog
