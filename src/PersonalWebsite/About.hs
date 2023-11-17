module PersonalWebsite.About (
    module PersonalWebsite.About.Pages,
    module PersonalWebsite.About.Capabilities,
    aboutHandler,
) where

import PersonalWebsite.About.API
import PersonalWebsite.About.Capabilities
import PersonalWebsite.About.Pages
import PersonalWebsite.Colors
import PersonalWebsite.Pages.Container (Tab (CV), renderSite)
import Polysemy
import Polysemy.Input
import Polysemy.Reader
import Relude hiding (MonadReader, Reader, ask, local)
import Servant

aboutHandler ::
    (Members [Reader ColorSeed, Input Int, Input ParsedCV] r) =>
    ServerT AboutAPI (Sem r)
aboutHandler = renderSite CV <=< aboutPage
