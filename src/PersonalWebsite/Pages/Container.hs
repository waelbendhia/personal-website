module PersonalWebsite.Pages.Container (
    renderSite,
    Tab (..),
    IsHXRequest (..),
) where

import qualified Clay as C
import qualified Data.Text as T
import Optics hiding (at)
import PersonalWebsite.API
import PersonalWebsite.API.CSS
import PersonalWebsite.About.API
import PersonalWebsite.Blogs.API
import PersonalWebsite.Colors
import PersonalWebsite.HTMX
import PersonalWebsite.Home.API
import PersonalWebsite.Internal
import PersonalWebsite.LiveReload
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
    | None
    deriving (Show, Eq, Enum, Bounded)

navItem :: Tab -> Tab -> Html
navItem at t =
    hxA href'
        ! A.class_ (if at == t then "active" else "")
        $ toMarkup
        $ T.toLower
        $ show t
  where
    href' = case t of
        Home -> fromLink $ apiLink (Proxy @HomeAPI)
        CV -> fromLink $ apiLink (Proxy @AboutAPI) Nothing
        Blog -> fromLink $ apiLink (Proxy @PageBlogsAPI) Nothing Nothing
        _ -> "/you-shouldn't-be-here"

siteHead :: (Members '[Reader ColorSeed] r) => Sem r Html
siteHead = do
    seed <- ask @ColorSeed
    st <- askCodeStyle
    varDeclarations <- declareVars <$> askColorPalette
    pure $ head do
        (script ! A.src "https://unpkg.com/htmx.org@1.9.8") pass
        (script ! A.src "https://unpkg.com/htmx.org/dist/ext/multi-swap.js") pass
        (script ! A.src "https://unpkg.com/htmx.org/dist/ext/sse.js") pass
        title "Wael's very own super special personal website"
        link
            ! A.id "favicon-link"
            ! A.rel "icon"
            ! A.href ("favicon.ico?tag=" <> fromText (toText seed))
        style ! A.id "var-declarations" $ toMarkup $ C.render varDeclarations
        style ! A.id "code-style" $ toMarkup $ toText $ styleToCss st
        meta
            ! A.name "viewport"
            ! A.content "width=device-width, initial-scale=1"
        style $ toMarkup $ C.render baseStyle
        link ! A.rel "stylesheet" ! A.href "/public/Iosevka.css"
        link ! A.rel "stylesheet" ! A.href "/public/IosevkaAile.css"
        link ! A.rel "stylesheet" ! A.href "/public/IosevkaEtoile.css"

navBar :: Tab -> Html
navBar at = div ! A.id "header-nav" ! A.class_ "nav" $ mapM_ (navItem at) [Home .. CV]

siteHeader :: Tab -> Html
siteHeader at = header do
    div ! A.class_ "title" $ h3 do
        "Wael's "
        span "very own super special personal "
        "website"
    navBar at

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
            hxA
                (fromLink (apiLink (Proxy @ColorGeneratorAPI) Nothing))
                "select a palette by seed"
            "."
        div do
            div do
                "Made with "
                (span ! hxExt "sse" ! sseConnect "/how-was-i-made" ! sseSwap "with")
                    $ text
                    $ fromMaybe ""
                    $ emojis
                    ^? ix (ind `mod` 7)
                " by "
                a ! A.href "https://wbd.tn" $ "Wael Ben Dhia"
            div do
                "Version: "
                a
                    ! A.href (fromString $ repoURL <> "/commit/" <> longHash)
                    ! A.target "_blank"
                    $ text shortHash
  where
    emojis = ["🤮", "😭", "😡", "🥴", "💔", "🤕", "🍆", "💦"]
    (shortHash, longHash) = $embedGitCommitHash
    repoURL = $embedRepositoryURL
    randomizePalette =
        button
            ! hxPost "/randomize"
            ! hxSwap "multi:#var-declarations:outerHtml,#code-style:outerHtml,#favicon-link:outerHtml"
            $ "Set a random color palette"

renderPage ::
    ( Members '[Reader ColorSeed, P.Input Int, P.Input UseLiveReload] r
    , ToMarkup content
    ) =>
    Tab ->
    content ->
    Sem r Html
renderPage at cnt = do
    head' <- siteHead
    footer' <- siteFooter
    UseLiveReload useLiveReload <- P.input
    pure $ docTypeHtml $ (html ! hxExt "multi-swap" ! hxBoost) do
        head'
        body do
            siteHeader at
            main ! A.id "main" $ toMarkup cnt
            footer'
            when useLiveReload liveReload

renderSite ::
    ( Members
        '[ Reader ColorSeed
         , P.Input Int
         , P.Input UseLiveReload
         , P.Input IsHXRequest
         ]
        r
    ) =>
    Tab ->
    Html ->
    Sem r Html
renderSite at cnt = do
    IsHXRequest isHX <- P.input @IsHXRequest
    if isHX
        then pure do
            navBar at
            (main ! A.id "main") cnt
        else renderPage at cnt
