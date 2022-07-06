module PersonalWebsite.Colors (
    askColorPalette,
    setTransparency,
    seedToPalette,
    module PersonalWebsite.Colors.API,
    module PersonalWebsite.Colors.CodeStyle,
    module PersonalWebsite.Colors.Palette,
) where

import Capability.Reader
import Clay.Color
import PersonalWebsite.Colors.API
import PersonalWebsite.Colors.CodeStyle
import PersonalWebsite.Colors.Palette
import Relude hiding (ask)
import System.Random

setTransparency :: Float -> Color -> Color
setTransparency a' (Rgba r' g' b' _) = Rgba r' g' b' a'
setTransparency a' (Hsla h' s' l' _) = Hsla h' s' l' a'
setTransparency _ o = o

seedToPalette :: Int -> Palette
seedToPalette = fst . randomPalette . mkStdGen

askColorPalette :: HasReader "colorSeed" Int m => m Palette
askColorPalette = seedToPalette <$> ask @"colorSeed"
