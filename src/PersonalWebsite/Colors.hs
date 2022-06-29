module PersonalWebsite.Colors (
    Palette (..),
    darkPalette,
    lightPalette,
    NordPalette (..),
    nordPalette,
    setTransparency,
    ColorMode (..),
    getColorPalette,
) where

import Capability.Reader
import Clay.Color
import Relude hiding (ask)

data ColorMode = Dark | Light deriving (Show)

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
        , nord3 = "#4c566a"
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

data Palette = Palette
    { bg :: Color
    , fg1 :: Color
    , fg2 :: Color
    , primary :: Color
    , highlight :: Color
    }

darkPalette :: Palette
darkPalette =
    Palette
        { bg = "#0E1428"
        , fg1 = nord6 nordPalette
        , fg2 = "#ABA8B2"
        , primary = nord13 nordPalette
        , highlight = nord7 nordPalette
        }

lightPalette :: Palette
lightPalette =
    Palette
        { bg = nord6 nordPalette
        , fg1 = "#0E1428"
        , fg2 = "#637081"
        , primary = "#FF8811"
        , highlight = "#23CE6B"
        }

setTransparency :: Float -> Color -> Color
setTransparency a' (Rgba r' g' b' _) = Rgba r' g' b' a'
setTransparency a' (Hsla h' s' l' _) = Hsla h' s' l' a'
setTransparency _ o = o

getColorPalette :: HasReader "colorMode" ColorMode m => m Palette
getColorPalette =
    ask @"colorMode" <&> \case
        Dark -> darkPalette
        Light -> lightPalette
