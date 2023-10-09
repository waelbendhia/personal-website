module PersonalWebsite.About.API (AboutAPI) where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html

type AboutAPI = "about" :> Get '[HTML] Html
