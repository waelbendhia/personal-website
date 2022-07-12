module PersonalWebsite.Blogs.API (PageBlogsAPI, TagsAPI, BlogAPI, BlogsAPI) where

import Relude
import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html

type PageBlogsAPI = "blog" :> QueryParam "page" Int :> QueryParam "tag" Text :> Get '[HTML] Html

type TagsAPI = "blog" :> "tags" :> Get '[HTML] Html

type BlogAPI = "blog" :> Capture "path" Text :> Get '[HTML] Html

type BlogsAPI = PageBlogsAPI :<|> TagsAPI :<|> BlogAPI
