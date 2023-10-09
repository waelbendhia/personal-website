module PersonalWebsite.API.CSS (mkBaseStyle) where

import Clay
import Optics hiding (pre, (#), (&), (|>))
import PersonalWebsite.Colors hiding (background)
import PersonalWebsite.Internal
import Polysemy
import Polysemy.Reader
import Relude hiding (Reader, ask, div, rem, (&), (**))

serifFont :: Css
serifFont = fontFamily ["Iosevka Etoile"] [serif]

sansSerifFont :: Css
sansSerifFont = fontFamily ["Iosevka Aile"] [sansSerif]

monospaceFont :: Css
monospaceFont = fontFamily ["Iosevka"] [monospace]

allLangs :: [Text]
allLangs =
    [ "json"
    , "yaml"
    , "javascript"
    , "typescript"
    , "jsx"
    , "tsx"
    , "go"
    , "lua"
    , "haskell"
    , "idris"
    , "elm"
    , "purescript"
    ]

mkCodeStyle :: (Member (Reader ColorSeed) r) => Sem r Css
mkCodeStyle =
    askColorPalette <&> \plt -> do
        code ? monospaceFont
        div # ".sourceCode" ? do
            border (px 1) solid (plt ^. #fg1)
            marginPx 24 0 32 0
            marginBottom (px 32)
            position relative
            forM_ allLangs $ \l ->
                star # byClass l ? ("font-feature-settings" -: fontFeature l)
            ".langtag" <? do
                position absolute
                right (px 0)
                top (px 0)
                paddingPx 3 3 4 8
            pre <? do
                marginPx 32 12 12 12
                mapM_ langBefore allLangs
                before & do
                    position absolute
                    left (px 0)
                    top (px 0)
                    paddingPx 3 8 4 3
  where
    langBefore l = fromString (toString $ "." <> l) <> before & content (stringContent l)
    fontFeature x
        | x `elem` ["javascript", "typescript", "jsx", "tsx", "go", "lua"] =
            "\"calt\" 0, \"CLIK\" 1"
        | x `elem` ["haskell", "idris", "elm", "purescript"] =
            "\"calt\" 0, \"HSKL\" 1"
        | otherwise =
            "\"calt\" 1"

flexRow :: Css
flexRow = do
    display flex
    flexDirection row
    alignItems center

mkBlogStyle :: (Member (Reader ColorSeed) r) => Sem r Css
mkBlogStyle =
    askColorPalette <&> \plt -> do
        a <> (".tag-header" ** ".tag") ? monospaceFont
        (".tag-header" ** ".tag") ? color (plt ^. #highlight)
        ".blog-list" ? do
            ".seperator" ? do
                background (plt ^. #fg2)
                height (px 4)
                sequence_ $ [marginLeft, marginRight, marginTop] <&> ($ px 16)
            margin (px 0) auto (px 0) auto
            maxWidth (px 768)
            ".blog-item" <? do
                ".summary" <? do
                    overflow hidden
                    position relative
                    maxHeight (px 352)
                    marginBottom (px 24)
                    before & do
                        content (stringContent "")
                        display block
                        height (px 64)
                        position absolute
                        mapM_ ($ px 0) [bottom, left]
                        background $
                            linearGradient
                                (angular $ deg 0)
                                [(plt ^. #bg, 0), (transparent, 100)]
                        width (pct 100)
                        zIndex 2
                ".link-row" <? do
                    flexRow
                    marginTop (px 12)
                    justifyContent flexEnd
                    fontSize (px 18)
                    a <? do
                        display block
                        paddingPx 4 4 4 4
                        fontWeight bold
        ".metadata" ? do
            flexRow
            justifyContent spaceBetween
            ".tags" <? do
                flexRow
                "gap" -: "8px"

tagsStyle :: Css
tagsStyle =
    ".tags" ? do
        flexRow
        fontSize (px 20)
        justifyContent center
        "gap" -: "16px"
        "flex-wrap" -: "wrap"

mkBaseStyle :: Members '[Reader ColorSeed] r => Sem r Css
mkBaseStyle = do
    plt <- askColorPalette
    styles <- sequence [mkBlogStyle, mkCodeStyle]
    pure do
        color (plt ^. #fg1)
        sequence_ styles
        tagsStyle
        input ? ".no-display" & display none
        ".toc" ? do
            padding (rem 0.5) (rem 1) (rem 1) (rem 1)
            li <? do
                forM_ [1 :: Int .. 6] \lvl ->
                    byClass ("level-" <> show lvl) & paddingLeft (rem $ fromIntegral lvl)
                marginBottom (rem 0.4)
                listStyleType none
                fontSize (rem 1.2)
        "input[type=text]" ? paddingLeft (px 12)
        "input[type=submit]" ? ":hover" & do
            color (plt ^. #primary)
            borderBottomColor (plt ^. #primary)
            cursor pointer
            transition "all" (ms 150) ease (ms 150)
        input <? do
            serifFont
            "border" -: "none"
            "background" -: "none"
            height (px 48)
            color (plt ^. #fg2)
            fontSize (px 18)
            fontWeight (weight 400)
            borderBottom (px 4) solid (plt ^. #fg2)
        ".lost" <> ".empty" <? do
            paddingTop (px 64)
            paddingBottom (px 64)
            textAlign center
            a ? fontWeight bold
        a ? do
            textDecoration underline
            transition "color" (ms 150) ease (ms 150)
            color (plt ^. #fg2)
            ":hover" & color (plt ^. #primary)
        h1 <> h2 <> h3 <> h4 <> header ? serifFont
        header ? do
            flexRow
            whiteSpace nowrap
            zIndex 4
            background $ setTransparency 0.5 (plt ^. #bg)
            height (px 48)
            lineHeight (px 48)
            paddingPx 0 32 0 32
            position sticky
            top (px 0)
            justifyContent spaceBetween
            maxWidth (px 1024)
            margin auto auto auto auto
            ".title" <? do
                flexRow
                height (px 48)
                "gap" -: "16px"
                form <? do
                    marginPx 0 0 0 0
                    height (px 48)
                    input <? width (px 48)
            h1 <? do
                fontSize (px 24)
                fontWeight (weight 500)
                marginPx 0 0 0 0
            div # ".nav" |> a <? do
                textDecoration none
                fontSize (px 18)
                fontWeight (weight 400)
                paddingPx 0 8 0 8
                height (px 44)
                display inlineBlock
                transition "all" (ms 150) ease (ms 150)
                ".active" & do
                    borderBottom (px 4) solid (plt ^. #fg2)
                    ":hover" & borderBottomColor (plt ^. #primary)
            h2 <? do
                flexShrink 1
                overflow hidden
                textOverflow overflowEllipsis
        body ? do
            background (plt ^. #bg)
            color (plt ^. #fg1)
            sansSerifFont
            marginPx 0 0 0 0
            div # ".content" <? do
                paddingTop (px 24)
                paddingBottom (px 24)
                paddingLeft (px 16)
                paddingRight (px 16)
                maxWidth (px 1024)
                margin (px 0) auto (px 0) auto
                p ? marginBottom (px 32)
                h1 <> h2 <> h3 <> h4 <> p <> li <> ".metadata" ? do
                    maxWidth (px 768)
                    marginLeft auto
                    marginRight auto
        ".intro-block" ? fontSize (px 18)
        ".pager" ? do
            flexRow
            height (px 96)
            justifyContent spaceBetween
            height (px 96)
            fontSize (px 18)
            a # ".disabled" <? do
                textDecoration lineThrough
                pointerEvents none
