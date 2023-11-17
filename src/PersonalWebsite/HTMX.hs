module PersonalWebsite.HTMX (
    hxTarget,
    hxGet,
    hxSwap,
    hxPushURL,
    hxA,
    hxPost,
    hxExt,
    hxBoost,
    IsHXRequest (..),
    sseSwap,
    sseConnect,
) where

import Relude
import Servant
import Text.Blaze
import Text.Blaze.Html5
import qualified Text.Blaze.XHtml5.Attributes as A

newtype IsHXRequest = IsHXRequest Bool
    deriving (FromHttpApiData)

hxTarget :: AttributeValue -> Attribute
hxTarget = customAttribute "hx-target"

hxGet :: AttributeValue -> Attribute
hxGet = customAttribute "hx-get"

hxSwap :: AttributeValue -> Attribute
hxSwap = customAttribute "hx-swap"

hxPost :: AttributeValue -> Attribute
hxPost = customAttribute "hx-post"

hxPushURL :: Attribute
hxPushURL = customAttribute "hx-push-url" "true"

hxExt :: AttributeValue -> Attribute
hxExt = customAttribute "hx-ext"

hxBoost :: Attribute
hxBoost = customAttribute "hx-boost" "true"

hxA :: AttributeValue -> Html -> Html
hxA href' =
    a
        ! A.href href'
        ! hxSwap "multi:#main:outerHTML,#header-nav:outerHTML"
        ! hxPushURL
        ! customAttribute "preload" "mouseover"

sseConnect :: AttributeValue -> Attribute
sseConnect = customAttribute "sse-connect"

sseSwap :: AttributeValue -> Attribute
sseSwap = customAttribute "sse-swap"
