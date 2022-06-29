module PersonalWebsite.API.Container (renderSite, Tab (..)) where

import Capability.Reader
import qualified Clay as C
import qualified Data.Text as T
import PersonalWebsite.API.CSS
import PersonalWebsite.Colors
import PersonalWebsite.Nord
import Relude hiding (ask, div, head, (**))
import Skylighting
import Text.Blaze
import qualified Text.Blaze.Html4.FrameSet.Attributes as A
import Text.Blaze.Html5

data Tab = Home | Blog | Toys | None deriving (Show, Eq)

navItem :: Tab -> Tab -> Html
navItem at t =
    a ! A.href (href' t) ! A.class_ (if at == t then "active" else "") $
        toMarkup $ T.toLower $ show t
  where
    href' Home = "/"
    href' Blog = "/blog"
    href' Toys = "/toys"
    href' _ = "/you-shouldn't-be-here"

siteHead :: (HasReader "colorMode" ColorMode m) => m Html
siteHead = do
    baseStyle <- mkBaseStyle
    mode' <- ask @"colorMode"
    pure . head $ do
        title "Wael's very own super special personal website"
        style . toMarkup . styleToCss $ case mode' of
            Dark -> nord
            Light -> kate
        style $ toMarkup $ C.render baseStyle
        link ! A.rel "stylesheet" ! A.href "https://cdn.jsdelivr.net/gh/aymanbagabas/iosevka-fonts@v6.1.2/dist/iosevka/iosevka.min.css"
        link ! A.rel "stylesheet" ! A.href "https://cdnjs.cloudflare.com/ajax/libs/Iosevka/6.0.0/iosevka-etoile/iosevka-etoile.min.css"

paletteToggle :: (HasReader "colorMode" ColorMode m) => m Html
paletteToggle = do
    m <- ask @"colorMode"
    pure $
        form ! A.method "POST" ! A.action "/toggle" $ do
            input ! A.type_ "submit" ! A.name "submit"
                ! A.value
                    ( case m of
                        Light -> "🌑"
                        Dark -> "🌕"
                    )

siteHeader :: HasReader "colorMode" ColorMode m => Tab -> m Html
siteHeader at = do
    pt <- paletteToggle
    pure . header $ do
        div ! A.class_ "title" $ do
            pt
            h3 "Wael's very own super special personal website"
        div ! A.class_ "nav" $ mapM_ (navItem at) [Home, Blog, Toys]

siteBody :: (HasReader "colorMode" ColorMode m, ToMarkup a) => Tab -> a -> m Html
siteBody at cnt = do
    sh <- siteHeader at
    pure . body $ do
        sh
        div ! A.class_ "content" $ toMarkup cnt

renderSite ::
    ( HasReader "colorMode" ColorMode m
    , ToMarkup content
    ) =>
    Tab ->
    content ->
    m Html
renderSite at cnt = do
    head' <- siteHead
    sb <- siteBody at cnt
    pure $
        html $ do
            head'
            sb
