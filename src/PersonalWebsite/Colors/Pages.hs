module PersonalWebsite.Colors.Pages (paletteHandler) where

import qualified Clay as C
import PersonalWebsite.API.CSS
import PersonalWebsite.Colors
import PersonalWebsite.Colors.API
import PersonalWebsite.HTMX
import PersonalWebsite.Internal
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Reader
import Relude hiding (runReader)
import Servant
import System.Random
import Text.Blaze.Html5 hiding (input)
import qualified Text.Blaze.Html5.Attributes as A
import Web.Cookie

sessionCookie :: ColorSeed -> SetCookie
sessionCookie seed =
    defaultSetCookie
        { setCookieName = "session-data"
        , setCookieValue = encodeUtf8 $ toText seed
        }

setCookieHeader :: ColorSeed -> v -> Headers '[Header "Set-Cookie" SetCookie] v
setCookieHeader = addHeader . sessionCookie

returnNewColorsOrThrow ::
    (Members '[Error ServerError, Input IsHXRequest] r) =>
    Maybe Text ->
    ColorSeed ->
    Sem r (SetPaletteHeaders Html)
returnNewColorsOrThrow ref s = do
    IsHXRequest isHXRequest <- input
    unless
        isHXRequest
        ( throw
            err301
                { errHeaders =
                    [ ("Set-Cookie", toHeader $ sessionCookie s)
                    , ("Location", encodeUtf8 $ fromMaybe "" ref)
                    ]
                }
        )
    runReader s $ do
        st <- askCodeStyle
        varDeclarations <- declareVars <$> askColorPalette
        pure $ setCookieHeader s do
            style
                ! A.id "var-declarations"
                $ toMarkup
                $ C.renderWith C.compact [] varDeclarations
            style
                ! A.id "code-style"
                $ toMarkup
                $ C.renderWith C.compact []
                $ styleToClay st
            link
                ! A.id "favicon-link"
                ! A.rel "icon"
                ! A.href ("favicon.ico?tag=" <> fromText (toText s))

randomizeHandler ::
    (Members '[Embed IO, Input IsHXRequest, Error ServerError] r) =>
    ServerT RandomizeAPI (Sem r)
randomizeHandler ref = do
    s <- ColorSeed . fst . random <$> liftIO initStdGen
    returnNewColorsOrThrow ref s

setSeedHandler ::
    (Members '[Embed IO, Input IsHXRequest, Error ServerError] r) =>
    ServerT SetSeedAPI (Sem r)
setSeedHandler ref s = returnNewColorsOrThrow ref (coerce s)

paletteHandler ::
    (Members '[Embed IO, Input IsHXRequest, Error ServerError] r) =>
    ServerT PaletteAPI (Sem r)
paletteHandler = setSeedHandler :<|> randomizeHandler
