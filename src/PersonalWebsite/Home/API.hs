module PersonalWebsite.Home.API (HomeAPI) where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html

type HomeAPI = Get '[HTML] Html
