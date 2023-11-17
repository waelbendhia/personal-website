module PersonalWebsite.Colors (
    askColorPalette,
    setTransparency,
    seedToPalette,
    module PersonalWebsite.Colors.CodeStyle,
    module PersonalWebsite.Colors.Palette,
    module PersonalWebsite.Colors.Data,
) where

import Clay.Color
import PersonalWebsite.Colors.CodeStyle
import PersonalWebsite.Colors.Data
import PersonalWebsite.Colors.Palette
import Polysemy
import Polysemy.Reader
import Relude hiding (Reader, ask, asks)
import System.Random

setTransparency :: Float -> Color -> Color
setTransparency a' (Rgba r' g' b' _) = Rgba r' g' b' a'
setTransparency a' (Hsla h' s' l' _) = Hsla h' s' l' a'
setTransparency _ o = o

seedToPalette :: ColorSeed -> Palette
seedToPalette = fst . randomPalette . mkStdGen . coerce

askColorPalette :: (Members '[Reader ColorSeed] r) => Sem r Palette
askColorPalette = asks seedToPalette
