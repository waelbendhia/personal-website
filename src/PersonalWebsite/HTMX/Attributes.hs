module PersonalWebsite.HTMX.Attributes (hxTarget, hxGet, hxSwap) where

import Text.Blaze

hxTarget :: AttributeValue -> Attribute
hxTarget = customAttribute "hx-target"

hxGet :: AttributeValue -> Attribute
hxGet = customAttribute "hx-get"

hxSwap :: AttributeValue -> Attribute
hxSwap = customAttribute "hx-swap"
