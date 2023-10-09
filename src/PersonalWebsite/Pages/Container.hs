module PersonalWebsite.Pages.Container (renderSite, Tab (..)) where

import qualified Clay as C
import qualified Data.Text as T
import PersonalWebsite.API
import PersonalWebsite.API.CSS
import PersonalWebsite.About.API
import PersonalWebsite.Blogs.API
import PersonalWebsite.Colors
import PersonalWebsite.Home.API
import PersonalWebsite.Internal
import PersonalWebsite.Toys.API
import Polysemy
import Polysemy.Reader
import Relude hiding (Reader, ask, div, head, runReader, (**))
import Skylighting
import Text.Blaze
import qualified Text.Blaze.Html4.FrameSet.Attributes as A
import Text.Blaze.Html5

data Tab = Home | About | Blog | Toys | None deriving (Show, Eq, Enum, Bounded)

navItem :: Tab -> Tab -> Html
navItem at t =
    a ! href' ! A.class_ (if at == t then "active" else "") $
        toMarkup $
            T.toLower $
                show t
  where
    href' = A.href $ case t of
        Home -> fromLink $ apiLink (Proxy @HomeAPI)
        About -> fromLink $ apiLink (Proxy @AboutAPI)
        Blog -> fromLink $ apiLink (Proxy @PageBlogsAPI) Nothing Nothing
        Toys -> fromLink $ apiLink (Proxy @ListToysAPI)
        _ -> "/you-shouldn't-be-here"

siteHead :: (Members '[Reader ColorSeed] r) => Sem r Html
siteHead = do
    baseStyle <- mkBaseStyle
    st <- askCodeStyle
    pure $ head do
        title "Wael's very own super special personal website"
        style $ toMarkup $ toText $ styleToCss st
        style $ toMarkup $ C.render baseStyle
        link ! A.rel "stylesheet" ! A.href "https://cdn.jsdelivr.net/gh/aymanbagabas/iosevka-fonts@v6.1.2/dist/iosevka/iosevka.min.css"
        link ! A.rel "stylesheet" ! A.href "https://cdnjs.cloudflare.com/ajax/libs/Iosevka/6.0.0/iosevka-etoile/iosevka-etoile.min.css"

randomizePalette :: Html
randomizePalette =
    form ! A.method "POST" ! A.action "/randomize" $
        input ! A.type_ "submit" ! A.name "submit" ! A.value "R"

siteHeader :: Tab -> Html
siteHeader at = header do
    div ! A.class_ "title" $ do
        randomizePalette
        h3 "Wael's very own super special personal website"
    div ! A.class_ "nav" $ mapM_ (navItem at) [Home .. Toys]

siteBody :: ToMarkup a => Tab -> a -> Html
siteBody at cnt = body do
    siteHeader at
    div ! A.class_ "content" $ toMarkup cnt

renderSite ::
    (Members '[Reader ColorSeed] r, ToMarkup content) =>
    Tab ->
    content ->
    Sem r Html
renderSite at cnt = do
    head' <- siteHead
    pure $ html do
        head'
        siteBody at cnt
