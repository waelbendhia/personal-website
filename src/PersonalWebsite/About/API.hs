module PersonalWebsite.About.API (AboutAPI) where

import Relude
import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html

type AboutAPI = "cv" :> QueryParam "tag" Text :> Get '[HTML] Html
