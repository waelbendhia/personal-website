module PersonalWebsite.About (
    module PersonalWebsite.About.Pages,
    module PersonalWebsite.About.Capabilities,
    aboutHandler,
) where

import Network.URI
import PersonalWebsite.About.API
import PersonalWebsite.About.Capabilities
import PersonalWebsite.About.Pages
import PersonalWebsite.Colors
import PersonalWebsite.HTMX
import PersonalWebsite.LiveReload
import PersonalWebsite.Pages.Container (Tab (CV), renderSite)
import Polysemy
import Polysemy.Input
import Polysemy.Reader
import Relude hiding (MonadReader, Reader, ask, local)
import Servant

aboutHandler ::
    ( Members
        [ Reader ColorSeed
        , Input Int
        , Input ParsedCV
        , Input IsHXRequest
        , Input UseLiveReload
        ]
        r
    ) =>
    ServerT AboutAPI (Sem r)
aboutHandler currentURL tag =
    case mfilter (== "/cv") mSourcePath of
        Just _ -> justTheProjects tag
        Nothing -> do
            cnt <- aboutPage tag
            renderSite CV cnt
  where
    mSourcePath = do
        unparsed <- toString <$> currentURL
        URI{uriPath = sourcePath} <- parseURI unparsed
        pure sourcePath
