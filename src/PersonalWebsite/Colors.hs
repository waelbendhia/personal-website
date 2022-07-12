module PersonalWebsite.Colors (
    askColorPalette,
    setTransparency,
    seedToPalette,
    module PersonalWebsite.Colors.CodeStyle,
    module PersonalWebsite.Colors.Palette,
    paletteHandler,
) where

import Capability.Reader
import Clay.Color
import PersonalWebsite.Colors.API
import PersonalWebsite.Colors.CodeStyle
import PersonalWebsite.Colors.Palette
import Relude hiding (ask)
import Servant
import System.Random
import Web.Cookie

setTransparency :: Float -> Color -> Color
setTransparency a' (Rgba r' g' b' _) = Rgba r' g' b' a'
setTransparency a' (Hsla h' s' l' _) = Hsla h' s' l' a'
setTransparency _ o = o

seedToPalette :: Int -> Palette
seedToPalette = fst . randomPalette . mkStdGen

askColorPalette :: HasReader "colorSeed" Int m => m Palette
askColorPalette = seedToPalette <$> ask @"colorSeed"

setPaletteHeaders :: Int -> Maybe Text -> v -> SetPaletteHeaders v
setPaletteHeaders colorSeed ref =
    let newCookie =
            defaultSetCookie
                { setCookieName = "session-data"
                , setCookieValue = show colorSeed
                }
     in addHeader newCookie . addHeader (fromMaybe "" ref)

paletteHandler :: MonadIO m => ServerT PaletteAPI m
paletteHandler = setSeedHandler :<|> randomizeHandler
  where
    setSeedHandler ref s = pure $ setPaletteHeaders (coerce s) ref ""
    randomizeHandler ref = do
        s <- fst . random <$> liftIO initStdGen
        pure $ setPaletteHeaders s ref ""
