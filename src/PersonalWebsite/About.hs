module PersonalWebsite.About (
    module PersonalWebsite.About.Pages,
    aboutHandler,
) where

import PersonalWebsite.About.API
import PersonalWebsite.About.Pages
import PersonalWebsite.Colors
import PersonalWebsite.Pages.Container (Tab (About), renderSite)
import Polysemy
import Polysemy.Input
import Polysemy.Reader
import Relude hiding (MonadReader, Reader, ask, local)
import Servant

aboutHandler :: (Members [Reader ColorSeed, Input Int] r) => ServerT AboutAPI (Sem r)
aboutHandler = renderSite About =<< aboutPage
