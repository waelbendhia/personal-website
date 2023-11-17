module PersonalWebsite.Pages.Container (renderSite, Tab (..)) where

import qualified Clay as C
import qualified Data.Text as T
import Optics hiding (at)
import PersonalWebsite.API
import PersonalWebsite.API.CSS
import PersonalWebsite.About.API
import PersonalWebsite.Blogs.API
import PersonalWebsite.Colors
import PersonalWebsite.Home.API
import PersonalWebsite.Internal
import PersonalWebsite.TH
import PersonalWebsite.Toys.API
import Polysemy
import qualified Polysemy.Input as P
import Polysemy.Reader
import Relude hiding (Reader, ask, div, head, runReader, span, (**))
import Skylighting
import Text.Blaze
import qualified Text.Blaze.Html4.FrameSet.Attributes as A
import Text.Blaze.Html5

data Tab
    = Home
    | Blog
    | CV
    | Toys
    | None
    deriving (Show, Eq, Enum, Bounded)

navItem :: Tab -> Tab -> Html
navItem at t =
    a
        ! href'
        ! A.class_ (if at == t then "active" else "")
        $ toMarkup
        $ T.toLower
        $ show t
  where
    href' = A.href $ case t of
        Home -> fromLink $ apiLink (Proxy @HomeAPI)
        CV -> fromLink $ apiLink (Proxy @AboutAPI) Nothing
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
        link
            ! A.rel "stylesheet"
            ! A.href "https://cdn.jsdelivr.net/gh/aymanbagabas/iosevka-fonts@v6.1.2/dist/iosevka/iosevka.min.css"
        link
            ! A.rel "stylesheet"
            ! A.href "https://cdnjs.cloudflare.com/ajax/libs/Iosevka/6.0.0/iosevka-etoile/iosevka-etoile.min.css"

siteHeader :: Tab -> Html
siteHeader at = header do
    div ! A.class_ "title" $ h3 "Wael's very own super special personal website"
    div ! A.class_ "nav" $ mapM_ (navItem at) [Home .. Toys]

siteBody :: (ToMarkup a) => Tab -> a -> Html
siteBody at cnt = body do
    siteHeader at
    main $ toMarkup cnt

siteFooter :: (Members '[P.Input Int] r) => Sem r Html
siteFooter = do
    ind <- P.input @Int
    pure $ footer do
        div $ span do
            "You can find me on "
            a ! A.href $myLinkedIn $ "LinkedIn"
            " or "
            a ! A.href $myGitHub $ "GitHub"
            "."
        div $ span do
            randomizePalette
            " or "
            a
                ! A.href (fromLink (apiLink (Proxy @ColorGeneratorAPI) Nothing))
                $ "select a palette by seed"
            "."
        div do
            div do
                "Made with "
                text $ fromMaybe "" $ emojis ^? ix (ind `mod` 7)
                " by "
                a ! A.href "https://wbd.tn" $ "Wael Ben Dhia"
            div do
                "Version: "
                a
                    ! A.href (fromString $ repoURL <> "/commit/" <> longHash)
                    ! A.target "_blank"
                    $ text shortHash
  where
    emojis = ["😭", "😡", "🥴", "💔", "🤕", "🍆", "💦"]
    (shortHash, longHash) = $embedGitCommitHash
    repoURL = $embedRepositoryURL
    randomizePalette =
        form
            ! A.method "POST"
            ! A.action "/randomize"
            $ input
            ! A.type_ "submit"
            ! A.name "submit"
            ! A.value "Click me to set a random color palette"

renderSite ::
    (Members '[Reader ColorSeed, P.Input Int] r, ToMarkup content) =>
    Tab ->
    content ->
    Sem r Html
renderSite at cnt = do
    head' <- siteHead
    footer' <- siteFooter
    pure $ html do
        head'
        siteBody at cnt
        footer'
