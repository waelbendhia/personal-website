{-# OPTIONS_GHC -Wno-deprecations #-}

module PersonalWebsite.Colors.Palette (
    Palette_ (..),
    Palette,
    randomPalette,
    clayToText,
) where

import Clay hiding (b, object, p, q, round, s)
import Data.Aeson
import Data.Bits
import qualified Data.Text as T
import Numeric
import Optics
import PersonalWebsite.Colors.Conversion
import PersonalWebsite.Colors.RandomGen
import PersonalWebsite.Internal
import PersonalWebsite.Random
import Relude
import System.Random

clayToText :: Color -> Text
clayToText (Rgba r g b _) =
    let unpadded = toText $ showHex (shift r 16 .|. shift g 8 .|. b) ""
     in "#" <> T.replicate (6 - T.length unpadded) "0" <> unpadded
clayToText (Hsla h s l _) =
    let (r, g, b) = hslToRgb (h, s, l)
     in clayToText $ rgba r g b 1
clayToText c = plain $ unValue $ value c

data Palette_ val = Palette
    { bg :: !val
    , bgTransparent :: !val
    , fg1 :: !val
    , fg2 :: !val
    , primary :: !val
    , highlight :: !val
    }

type Palette = Palette_ Color

makeFieldLabelsWith noPrefixFieldLabels ''Palette_

instance Functor Palette_ where
    fmap f plt =
        Palette
            { bg = f (plt ^. #bg)
            , bgTransparent = f (plt ^. #bgTransparent)
            , fg1 = f (plt ^. #fg1)
            , fg2 = f (plt ^. #fg2)
            , primary = f (plt ^. #primary)
            , highlight = f (plt ^. #highlight)
            }

instance ToJSON Palette where
    toJSON p =
        object
            [ "bg" .= clayToText (p ^. #bg)
            , "fg1" .= clayToText (p ^. #fg1)
            , "fg2" .= clayToText (p ^. #fg2)
            , "primary" .= clayToText (p ^. #primary)
            , "highlight" .= clayToText (p ^. #highlight)
            ]

relativeLuminanceHSL :: (Integer, Float, Float) -> Float
relativeLuminanceHSL c =
    ( (0.2126 * fromIntegral r)
        + (0.7152 * fromIntegral g)
        + (0.0722 * fromIntegral b)
    )
        / 255
  where
    (r, g, b) = hslToRgb c

adjustContrast ::
    (Integer, Float, Float) ->
    (Integer, Float, Float) ->
    (Integer, Float, Float)
adjustContrast bg' fg = adjust fg
  where
    lumBG = relativeLuminanceHSL bg'
    lumFG = relativeLuminanceHSL fg
    adjust fg'@(h, s, l)
        | ratio >= 4.5 = fg'
        | l >= 90 = (h, s, 90)
        | l <= 10 = (h, s, 10)
        | otherwise =
            adjust (h, s, if lumBG < lumFG then l + 5 else l - 5)
      where
        ratio =
            if lumBG < lumFG
                then (relativeLuminanceHSL fg' + 0.05) / (lumBG + 0.05)
                else (lumBG + 0.05) / (relativeLuminanceHSL fg' + 0.05)

randomPalette :: (RandomGen g) => g -> (Palette, g)
randomPalette g = withRandom g do
    bg'@(bgh, bgs, bgl) <- randomRM ((0, 0, 0), (255, 100, 100))
    let lumBG = relativeLuminanceHSL (bgh, bgs, bgl)
        lShift1 = if lumBG > 0.5 then -0.6 else 0.6
        lShift2 = if lumBG > 0.5 then -0.4 else 0.4
    ( Palette
            (toClay bg')
            (Hsla bgh bgs bgl 0.7)
            `on4` (toClay . adjustContrast bg')
        )
        <$> randomHSLAColorJitter
            (bgh + 120, lerpF (-0.5) bgs, lerpF lShift1 bgl)
            (10, 10, 5)
        <*> randomHSLAColorJitter
            (bgh + 140, bgs, lerpF lShift2 bgl)
            (10, 10, 10)
        <*> randomHSLAColorJitter
            (bgh + 90, bgs, lerpF lShift1 bgl)
            (10, 10, 5)
        <*> randomHSLAColorJitter
            (bgh + 180, lerpF 0.3 bgs, lerpF lShift1 bgl)
            (10, 10, 10)
  where
    toClay (h, s, l) = Hsla h s l 1
    lerpF r v =
        if r < 0
            then v + (-v) * abs r
            else v + (100 - v) * r
