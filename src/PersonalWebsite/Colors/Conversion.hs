module PersonalWebsite.Colors.Conversion where

import Clay hiding (b, p, q, round, s)
import Data.Bits
import Graphics.Image
import PersonalWebsite.Internal
import Relude
import qualified Skylighting as Sk

hslToRgb :: (Integer, Float, Float) -> (Integer, Integer, Integer)
hslToRgb (h, s, l)
    | s' == 0 = let v = asVal s' in (v, v, v)
    | otherwise =
        ( asVal $ hueToRGB $ h' + (1 / 3)
        , asVal $ hueToRGB h'
        , asVal $ hueToRGB $ h' - (1 / 3)
        )
  where
    asVal v = round $ v * 255
    h' = fromIntegral h / 360
    l' = l / 100
    s' = s / 100
    q = if l' < 0.5 then l' * (1 + s') else l' + s' - (l' * s')
    p = (2 * l') - q
    hueToRGB t =
        let t'
                | t < 0 = t + 1
                | t > 1 = t - 1
                | otherwise = t
         in if
                | t' < 1 / 6 -> p + ((q - p) * 6 * t')
                | t' < 1 / 2 -> q
                | t' < 2 / 3 -> p + ((q - p) * (2 / 3 - t') * 6)
                | otherwise -> p

hslToSkylighting :: (Integer, Float, Float) -> Maybe Sk.Color
hslToSkylighting col' = clayToSkylighting $ rgb r g b
  where
    (r, g, b) = hslToRgb col'

clayToSkylighting :: Color -> Maybe Sk.Color
clayToSkylighting (Rgba r g b _) =
    Sk.toColor @Int $ fromInteger $ shift r 16 .|. shift g 8 .|. b
clayToSkylighting (Hsla h s l _) = hslToSkylighting (h, s, l)
clayToSkylighting _ = Nothing

skylightingToClay :: Sk.Color -> Color
skylightingToClay (Sk.RGB r g b) = (rgb `on3` fromIntegral) r g b

clayColorToPixelRGB :: Color -> Pixel RGB Integer
clayColorToPixelRGB (Rgba r g b _) = PixelRGB r g b
clayColorToPixelRGB (Hsla h s l _) =
    let (r, g, b) = hslToRgb (h, s, l)
     in clayColorToPixelRGB $ rgba r g b 1
clayColorToPixelRGB _ = PixelRGB 0 0 0
