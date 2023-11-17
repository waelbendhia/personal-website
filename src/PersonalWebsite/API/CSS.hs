module PersonalWebsite.API.CSS (mkBaseStyle) where

import Clay as Cl
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
            marginRem 1.5 0 2 0
            marginBottom (rem 2)
            position relative
            forM_ allLangs $ \l ->
                star # byClass l ? ("font-feature-settings" -: fontFeature l)
            ".langtag" <? do
                position absolute
                right (px 0)
                top (px 0)
                paddingRem 0.25 0.25 0.25 0.5
            pre <? do
                marginRem 2 0.75 0.75 0.75
                mapM_ langBefore allLangs
                before & do
                    position absolute
                    left (px 0)
                    top (px 0)
                    paddingRem 0.25 0.5 0.25 0.25
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
                height (rem 0.25)
                sequence_ $ [marginLeft, marginRight, marginTop] <&> ($ rem 1)
            margin (px 0) auto (px 0) auto
            maxWidth (px 768)
            ".blog-item" <? do
                ".summary" <? do
                    overflow hidden
                    position relative
                    maxHeight (px 352)
                    marginBottom (rem 1.5)
                    before & do
                        content (stringContent "")
                        display block
                        height (rem 4)
                        position absolute
                        mapM_ ($ px 0) [bottom, left]
                        background
                            $ linearGradient
                                (angular $ deg 0)
                                [(plt ^. #bg, 0), (transparent, 100)]
                        width (pct 100)
                        zIndex 2
                ".link-row" <? do
                    flexRow
                    marginTop (rem 0.75)
                    justifyContent flexEnd
                    fontSize (rem 1.125)
                    a <? do
                        display block
                        paddingRem 0.25 0.25 0.25 0.25
                        fontWeight bold
        ".metadata" ? do
            flexRow
            justifyContent spaceBetween
            ".tags" <? do
                flexRow
                "gap" -: "0.5rem"

tagsStyle :: Css
tagsStyle =
    ".tags" ? do
        flexRow
        fontSize (rem 1.25)
        justifyContent center
        "gap" -: "16px"
        "flex-wrap" -: "wrap"

mkBaseStyle :: (Members '[Reader ColorSeed] r) => Sem r Css
mkBaseStyle = do
    plt <- askColorPalette
    styles <- sequence [mkBlogStyle, mkCodeStyle]
    let linkStyle = do
            textDecoration underline
            transition "color" (ms 150) ease (ms 150)
            color (plt ^. #fg2)
            ":hover" & color (plt ^. #primary)
    pure do
        color (plt ^. #fg1)
        sequence_ styles
        tagsStyle
        footer ? do
            paddingRem 0 1 0 1
            marginRem 3 0 1.5 0
            div <? do
                display flex
                flexDirection row
                justifyContent spaceBetween
                marginLeft auto
                marginRight auto
                marginTop (rem 1)
                maxWidth (px 1024)
            form ? do
                display inline
                marginBottom (rem 0)
                input <? do
                    borderBottom (rem 0) none transparent
                    paddingLeft (rem 0)
                    paddingRight (rem 0)
                    height auto
                    fontSize (rem 1)
                    sansSerifFont
                    linkStyle
        input ? ".no-display" & display none
        ".toc" ? do
            paddingRem 0.5 1 1 1
            li <? do
                forM_ [1 :: Int .. 6] \lvl ->
                    byClass ("level-" <> show lvl) & paddingLeft (rem $ fromIntegral lvl)
                marginBottom (rem 0.4)
                listStyleType none
                fontSize (rem 1.2)
        "input[type=text]" ? paddingLeft (rem 0.75)
        "input[type=submit]" ? ":hover" & do
            color (plt ^. #primary)
            borderBottomColor (plt ^. #primary)
            cursor pointer
            transition "all" (ms 150) ease (ms 150)
        input <? do
            serifFont
            "border" -: "none"
            "background" -: "none"
            height (rem 3)
            color (plt ^. #fg2)
            fontSize (rem 1.125)
            fontWeight (weight 400)
            borderBottom (rem 0.25) solid (plt ^. #fg2)
        ".lost" <> ".empty" <? do
            paddingTop (rem 4)
            paddingBottom (rem 4)
            textAlign center
            a ? fontWeight bold
        a ? linkStyle
        h1 <> h2 <> h3 <> h4 <> header ? serifFont
        header ? do
            flexRow
            whiteSpace nowrap
            zIndex 4
            background $ setTransparency 0.5 (plt ^. #bg)
            height (rem 3)
            lineHeight (rem 3)
            paddingRem 0 2 0 2
            position sticky
            top (px 0)
            justifyContent spaceBetween
            maxWidth (px 1024)
            margin auto auto auto auto
            ".title" <? do
                flexRow
                height (rem 3)
                "gap" -: "1 rem"
                form <? do
                    marginRem 0 0 0 0
                    height (rem 3)
                    input <? width (rem 3)
            h1 <? do
                fontSize (rem 1.5)
                fontWeight (weight 500)
                marginRem 0 0 0 0
            div # ".nav" |> a <? do
                textDecoration none
                fontSize (rem 1.125)
                fontWeight (weight 400)
                paddingRem 0 0.5 0 0.5
                height (rem 2.75)
                display inlineBlock
                transition "all" (ms 150) ease (ms 150)
                ".active" & do
                    borderBottom (rem 0.25) solid (plt ^. #fg2)
                    ":hover" & borderBottomColor (plt ^. #primary)
            h2 <? do
                flexShrink 1
                overflow hidden
                textOverflow overflowEllipsis
        body ? do
            background (plt ^. #bg)
            color (plt ^. #fg1)
            sansSerifFont
            marginRem 0 0 0 0
        main_ <? do
            paddingRem 1.5 1 1.5 1
            maxWidth (px 1024)
            margin (px 0) auto (px 0) auto
            p ? marginBottom (rem 2)
            h1 <> h2 <> h3 <> h4 <> p <> li <> ".metadata" ? do
                maxWidth (px 768)
                marginLeft auto
                marginRight auto
        ".intro-block" ? fontSize (rem 1.125)
        ".pager" ? do
            flexRow
            height (rem 6)
            justifyContent spaceBetween
            height (rem 6)
            fontSize (rem 1.125)
            a # ".disabled" <? do
                textDecoration lineThrough
                pointerEvents none
