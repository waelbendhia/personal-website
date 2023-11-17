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

randomPalette :: (RandomGen g) => g -> (Palette, g)
randomPalette g = withRandom g do
    (bgh, bgs, bgl) <- randomRM ((0, 0, 0), (255, 100, 100))
    Palette
        (Hsla bgh bgs bgl 1)
        (Hsla bgh bgs bgl 0.7)
        <$> randomHSLAColorJitter (bgh + 120, bgs / 2, bgl + 50) (10, 10, 5)
        <*> randomHSLAColorJitter (bgh + 120, bgs, bgl + 30) (10, 10, 10)
        <*> randomHSLAColorJitter (bgh + 90, bgs, bgl + 50) (10, 10, 5)
        <*> randomHSLAColorJitter (bgh + 180, 80, bgl) (10, 10, 10)
