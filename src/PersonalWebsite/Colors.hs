module PersonalWebsite.Colors (
    askColorPalette,
    setTransparency,
    seedToPalette,
    module PersonalWebsite.Colors.CodeStyle,
    module PersonalWebsite.Colors.Palette,
    module PersonalWebsite.Colors.Data,
    paletteHandler,
) where

import Clay.Color
import PersonalWebsite.Colors.API
import PersonalWebsite.Colors.CodeStyle
import PersonalWebsite.Colors.Data
import PersonalWebsite.Colors.Palette
import Polysemy
import Polysemy.Reader
import Relude hiding (Reader, ask, asks)
import Servant
import System.Random
import Web.Cookie

setTransparency :: Float -> Color -> Color
setTransparency a' (Rgba r' g' b' _) = Rgba r' g' b' a'
setTransparency a' (Hsla h' s' l' _) = Hsla h' s' l' a'
setTransparency _ o = o

seedToPalette :: ColorSeed -> Palette
seedToPalette = fst . randomPalette . mkStdGen . coerce

askColorPalette :: Members '[Reader ColorSeed] r => Sem r Palette
askColorPalette = asks seedToPalette

setPaletteHeaders :: ColorSeed -> Maybe Text -> v -> SetPaletteHeaders v
setPaletteHeaders colorSeed ref =
    let newCookie =
            defaultSetCookie
                { setCookieName = "session-data"
                , setCookieValue = encodeUtf8 $ toText colorSeed
                }
     in addHeader newCookie . addHeader (fromMaybe "" ref)

paletteHandler :: MonadIO m => ServerT PaletteAPI m
paletteHandler = setSeedHandler :<|> randomizeHandler
  where
    setSeedHandler ref s = pure $ setPaletteHeaders (coerce s) ref ""
    randomizeHandler ref = do
        s <- ColorSeed . fst . random <$> liftIO initStdGen
        pure $ setPaletteHeaders s ref ""
