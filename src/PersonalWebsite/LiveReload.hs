module PersonalWebsite.LiveReload (
    liveReload,
    LiveReloadAPI,
    liveReloadHandler,
    ShouldRefresh (..),
    UseLiveReload (..),
    pollingInterval,
) where

import Katip
import PersonalWebsite.Katip
import Polysemy
import Polysemy.Input
import Relude hiding (div)
import Servant
import Text.Blaze.Html5 hiding (embed, input)
import qualified Text.Blaze.Html5.Attributes as A

type LiveReloadAPI =
    "live-reload"
        :> Get '[JSON] (Headers '[Header "HX-Refresh" Bool] Text)

newtype UseLiveReload = UseLiveReload Bool

data ShouldRefresh = ShouldRefresh

pollingInterval :: Int
pollingInterval = 2

liveReloadHandler ::
    (Members '[Input (Maybe ShouldRefresh), Embed IO, Katiper] r) =>
    ServerT LiveReloadAPI (Sem r)
liveReloadHandler = do
    shouldRefresh <- isJust <$> input @(Maybe ShouldRefresh)
    if shouldRefresh
        then $logTM InfoS "got refresh signal"
        else $logTM InfoS "not refreshing"
    pure (addHeader shouldRefresh "hello")

liveReload :: Html
liveReload =
    div
        ! A.class_ "live-reload"
        ! customAttribute "hx-get" "/live-reload"
        ! customAttribute
            "hx-trigger"
            ("every " <> show pollingInterval <> "s")
        $ pass
