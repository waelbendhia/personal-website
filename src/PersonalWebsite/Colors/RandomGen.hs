module PersonalWebsite.Colors.RandomGen where

import Data.Fixed
import PersonalWebsite.Random
import Relude
import qualified Skylighting as Sk
import System.Random

type HSL = (Integer, Float, Float)

randomHSLColorR :: (RandomGen g) => (HSL, HSL) -> RandomT g HSL
randomHSLColorR bs = do
    (h', s', l') <- randomRM bs
    pure (h' `mod` 360, s' `mod'` 100, l' `mod'` 100)

randomHSLColor :: (RandomGen g) => RandomT g HSL
randomHSLColor = randomHSLColorR ((0, 0, 0), (360, 100, 100))

randomHSLAColorJitter :: (RandomGen g) => HSL -> HSL -> RandomT g HSL
randomHSLAColorJitter (h', s', l') (jh, js, jl) =
    randomHSLColorR ((h' - jh, s' - js, l' - jl), (h' + jh, s' + js, l' + jl))

randomSKColor :: (RandomGen g) => RandomT g (Maybe Sk.Color)
randomSKColor = Sk.toColor <$> randomRM @Int (0, 0xFFFFFF)
