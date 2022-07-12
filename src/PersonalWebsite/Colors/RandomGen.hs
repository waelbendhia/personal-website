module PersonalWebsite.Colors.RandomGen where

import qualified Clay as Cl
import Data.Fixed
import PersonalWebsite.Random
import Relude
import qualified Skylighting as Sk
import System.Random

type HSL = (Integer, Float, Float)

randomHSLAColorR :: RandomGen g => (HSL, HSL) -> RandomT g Cl.Color
randomHSLAColorR bs = do
    (h', s', l') <- randomRM bs
    pure $ Cl.Hsla (h' `mod` 360) (s' `mod'` 100) (l' `mod'` 100) 1

randomHSLAColor :: RandomGen g => RandomT g Cl.Color
randomHSLAColor = randomHSLAColorR ((0, 0, 0), (360, 100, 100))

randomHSLAColorJitter :: RandomGen g => HSL -> HSL -> RandomT g Cl.Color
randomHSLAColorJitter (h', s', l') (jh, js, jl) =
    randomHSLAColorR ((h' - jh, s' - js, l' - jl), (h' + jh, s' + js, l' + jl))

randomSKColor :: (RandomGen g) => RandomT g (Maybe Sk.Color)
randomSKColor = Sk.toColor <$> randomRM @Int (0, 0xFFFFFF)
