module PersonalWebsite.About.Pages (aboutPage) where

import Polysemy
import Relude hiding (div)
import Text.Blaze.XHtml5

aboutPage :: Sem r Html
aboutPage = pure $ div "about me"
