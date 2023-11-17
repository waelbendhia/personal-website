module PersonalWebsite.Pages (
    lostPage,
    module PersonalWebsite.Pages.Container,
) where

import PersonalWebsite.API
import PersonalWebsite.HTMX
import PersonalWebsite.Home.API
import PersonalWebsite.Internal
import PersonalWebsite.Pages.Container
import Relude hiding (div, span)
import Text.Blaze.Html
import Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A

lostPage :: Html
lostPage = (div ! A.class_ "lost") $ p do
    span "You seem lost, why don't you navigate your way back "
    hxA (fromLink $ apiLink (Proxy @HomeAPI)) "home"
    span "."
