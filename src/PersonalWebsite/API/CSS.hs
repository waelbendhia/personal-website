{-# LANGUAGE NamedFieldPuns #-}

module PersonalWebsite.API.CSS (mkBaseStyle) where

import Capability.Reader
import Clay
import PersonalWebsite.Colors
import Relude hiding (ask, div, (&), (**))

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

codeStyle :: Css
codeStyle = do
    code ? monospaceFont
    div # ".sourceCode" ? do
        border (px 1) solid (nord3 nordPalette)
        margin (px 24) (px 0) (px 32) (px 0)
        marginBottom (px 32)
        position relative
        mapM_
            (\l -> star # byClass l ? do "font-feature-settings" -: fontFeature l)
            allLangs
        ".langtag" <? do
            position absolute
            right (px 0)
            top (px 0)
            padding (px 3) (px 3) (px 4) (px 8)
        pre <? do
            margin (px 32) (px 12) (px 12) (px 12)
            mapM_ langBefore allLangs
            before & do
                position absolute
                left (px 0)
                top (px 0)
                padding (px 3) (px 8) (px 4) (px 3)
  where
    langBefore l = fromString (toString $ "." <> l) <> before & content (stringContent l)
    fontFeature x
        | x `elem` ["javascript", "typescript", "jsx", "tsx", "go", "lua"] =
            "\"calt\" 0, \"CLIK\" 1"
        | x `elem` ["haskell", "idris", "elm", "purescript"] =
            "\"calt\" 0, \"HSKL\" 1"
        | otherwise =
            "\"calt\" 1"

mkBlogStyle :: (HasReader "colorMode" ColorMode m) => m Css
mkBlogStyle = do
    Palette{bg, fg2, highlight} <- getColorPalette
    pure $ do
        a <> (".tag-header" ** ".tag") ? monospaceFont
        (".tag-header" ** ".tag") ? color highlight
        ".blog-list" ? do
            ".seperator" ? do
                background fg2
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
                                [(bg, 0), (transparent, 100)]
                        width (pct 100)
                        zIndex 2
                ".link-row" <? do
                    marginTop (px 12)
                    display flex
                    flexDirection row
                    justifyContent flexEnd
                    fontSize (px 18)
                    a <? do
                        display block
                        padding (px 4) (px 4) (px 4) (px 4)
                        fontWeight bold
        ".metadata" ? do
            display flex
            flexDirection row
            justifyContent spaceBetween
            ".tags" <? do
                display flex
                flexDirection row
                "gap" -: "8px"

tagsStyle :: Css
tagsStyle =
    ".tags" ? do
        display flex
        flexDirection row
        fontSize (px 20)
        justifyContent center
        "gap" -: "16px"
        "flex-wrap" -: "wrap"

mkBaseStyle :: HasReader "colorMode" ColorMode m => m Css
mkBaseStyle = do
    Palette{bg, fg1, fg2, primary} <- getColorPalette
    blogStyle <- mkBlogStyle
    pure $ do
        blogStyle
        codeStyle
        tagsStyle
        ".lost" <> ".empty" <? do
            paddingTop (px 64)
            paddingBottom (px 64)
            textAlign center
            a ? fontWeight bold
        a ? do
            textDecoration none
            transition "color" (ms 150) ease (ms 150)
            color fg2
            ":hover" & color primary
        h1 <> h2 <> h3 <> h4 <> header ? serifFont
        header ? do
            whiteSpace nowrap
            zIndex 4
            background $ setTransparency 0.5 bg
            height (px 48)
            lineHeight (px 48)
            padding (px 0) (px 32) (px 0) (px 32)
            position sticky
            top (px 0)
            display flex
            flexDirection row
            justifyContent spaceBetween
            alignItems center
            maxWidth (px 1024)
            margin auto auto auto auto
            ".title" <? do
                height (px 48)
                display flex
                flexDirection row
                alignItems center
                "gap" -: "16px"
                form <? do
                    margin (px 0) (px 0) (px 0) (px 0)
                    height (px 48)
                    input <? do
                        "border" -: "none"
                        "background" -: "none"
                        height (px 48)
                        width (px 48)
                        cursor pointer
                        transition "all" (ms 150) ease (ms 150)
                        borderBottom (px 4) solid fg2
                        ":hover" & borderBottomColor primary
            h1 <? do
                fontSize (px 24)
                fontWeight (weight 500)
                margin (px 0) (px 0) (px 0) (px 0)
            div # ".nav" |> a <? do
                fontSize (px 18)
                fontWeight (weight 400)
                padding (px 0) (px 8) (px 0) (px 8)
                height (px 44)
                display inlineBlock
                transition "all" (ms 150) ease (ms 150)
                ".active" & do
                    borderBottom (px 4) solid fg2
                    ":hover" & borderBottomColor primary
            h2 <? do
                flexShrink 1
                overflow hidden
                textOverflow overflowEllipsis
        body ? do
            background bg
            color fg1
            sansSerifFont
            margin (px 0) (px 0) (px 0) (px 0)
            div # ".content" <? do
                paddingTop (px 24)
                paddingLeft (px 16)
                paddingRight (px 16)
                maxWidth (px 1024)
                margin (px 0) auto (px 0) auto
                p ? marginBottom (px 32)
                h1 <> h2 <> h3 <> h4 <> p <> ".metadata" ? do
                    maxWidth (px 768)
                    marginLeft auto
                    marginRight auto
        ".intro-block" ? do
            fontSize (px 18)
        ".pager" ? do
            height (px 96)
            display flex
            justifyContent spaceBetween
            height (px 96)
            alignItems center
            fontSize (px 18)
            a # ".disabled" <? do
                textDecoration lineThrough
                pointerEvents none
