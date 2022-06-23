{-# LANGUAGE NamedFieldPuns #-}

module PersonalWebsite.API.CSS (baseStyle, darkPalette) where

import Clay
import Relude hiding (div, (&))

data Palette = Palette
    { bg :: Color
    , fg1 :: Color
    , fg2 :: Color
    , primary :: Color
    }

darkPalette :: Palette
darkPalette =
    Palette
        { bg = "#0E1428"
        , fg1 = "#FFFFFF"
        , fg2 = "#ABA8B2"
        , primary = "#F18805"
        }

data NordPalette = NordPalette
    { nord0 :: !Color
    , nord1 :: !Color
    , nord2 :: !Color
    , nord3 :: !Color
    , nord4 :: !Color
    , nord5 :: !Color
    , nord6 :: !Color
    , nord7 :: !Color
    , nord8 :: !Color
    , nord9 :: !Color
    , nord10 :: !Color
    , nord11 :: !Color
    , nord12 :: !Color
    , nord13 :: !Color
    , nord14 :: !Color
    , nord15 :: !Color
    }

nordPalette :: NordPalette
nordPalette =
    NordPalette
        { nord0 = "#2e3440"
        , nord1 = "#3b4252"
        , nord2 = "#434c5e"
        , -- asdasd
          nord3 = "#4c566a"
        , nord4 = "#d8dee9"
        , nord5 = "#e5e9f0"
        , nord6 = "#eceff4"
        , nord7 = "#8fbcbb"
        , nord8 = "#88c0d0"
        , nord9 = "#81a1c1"
        , nord10 = "#5e81ac"
        , nord11 = "#bf616a"
        , nord12 = "#d08770"
        , nord13 = "#ebcb8b"
        , nord14 = "#a3be8c"
        , nord15 = "#b48ead"
        }

baseStyle :: Palette -> Css
baseStyle Palette{bg, fg1, fg2, primary} = do
    a ? do
        textDecoration none
        transition "color" (ms 150) ease (ms 150)
        color fg2
        ":hover" & color primary
    div # ".sourceCode" ? do
        border (px 1) solid (nord3 nordPalette)
        background (nord0 nordPalette)
        fontFamily ["Iosevka"] [monospace]
        color (nord6 nordPalette)
        margin (px 24) (px 0) (px 24) (px 0)
        position relative
        ".langtag" <? do
            position absolute
            right (px 0)
            top (px 0)
            padding (px 3) (px 3) (px 4) (px 8)
            background (nord3 nordPalette)
        pre <? do
            margin (px 12) (px 12) (px 12) (px 12)
            ".json" <> before & content (stringContent "json")
            ".haskell" <> before & content (stringContent "haskell")
            before & do
                position absolute
                right (px 0)
                top (px 0)
                padding (px 3) (px 3) (px 4) (px 8)
                background (nord3 nordPalette)
    h1 <> h2 <> h3 <> h4 <> header ? fontFamily ["Iosevka Etoile"] [serif]
    header ? do
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
            ".active" & borderBottom (px 4) solid fg2
    ".blog-list" ? do
        margin (px 0) auto (px 0) auto
        maxWidth (px 768)
        ".blog-item" <? do
            ".summary" <? do
                overflow hidden
                position relative
                maxHeight (px 256)
                marginBottom (px 24)
                before & do
                    content (stringContent "")
                    display block
                    height (px 64)
                    position absolute
                    bottom (px 0)
                    left (px 0)
                    background (linearGradient (angular $ deg 0) [(bg, 0), (transparent, 100)])
                    width (pct 100)
                    zIndex 2
            ".link-row" <? do
                display flex
                flexDirection row
                justifyContent flexEnd
                a <? do
                    display block
                    padding (px 4) (px 4) (px 4) (px 4)
                    fontWeight bold

    body ? do
        background bg
        color fg1
        fontFamily ["Iosevka Aile"] [sansSerif]
        margin (px 0) (px 0) (px 0) (px 0)
        div # ".content" <? do
            paddingTop (px 24)
            paddingLeft (px 16)
            paddingRight (px 16)
            maxWidth (px 1024)
            margin (px 0) auto (px 0) auto
            h1 <> h3 <> p <? do
                maxWidth (px 768)
                margin auto auto auto auto
