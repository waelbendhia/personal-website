module PersonalWebsite.Handlers (API, server, api) where

import Control.Concurrent
import Graphics.Image
import Network.HTTP.Types hiding (Header)
import Network.Wai (responseLBS)
import Optics
import PersonalWebsite.API
import PersonalWebsite.About
import PersonalWebsite.Blogs
import PersonalWebsite.Colors
import PersonalWebsite.Colors.Conversion
import PersonalWebsite.Colors.Pages
import PersonalWebsite.Cookies
import PersonalWebsite.Home
import PersonalWebsite.Pages
import PersonalWebsite.Pandoc
import PersonalWebsite.Toys
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Reader
import Relude hiding (MonadReader, Reader, ask, local, runReader)
import Servant
import Servant.Types.SourceT
import System.Random
import Text.Blaze.Renderer.Utf8

streamEmojis :: ServerT StreamojiAPI (Sem r)
streamEmojis = pure $ fromStepT streamer
  where
    emojis = ["🤮", "😭", "😡", "🥴", "💔", "🤕", "🍆", "💦"]
    streamer =
        Effect $ do
            threadDelay 5000000
            ind <- randomRIO @Int (0, 7)
            pure $ Yield (Event "with" $ fromMaybe "💔" (emojis ^? ix ind)) streamer

server ::
    ( Members
        [ Blogs
        , Render
        , Input Tags
        , Input Int
        , Embed IO
        , Input ParsedCV
        , Error ServerError
        ]
        r
    ) =>
    Text ->
    ServerT API (Sem r)
server publicFolder sess isHXRequest =
    hoistServer
        (Proxy @APIWithoutPalette)
        (runReader seed' . runInputConst isHXRequest')
        $ homeHandler
        :<|> aboutHandler
        :<|> blogsHandler
        :<|> toysHandler
        :<|> paletteHandler
        :<|> faviconHandler
        :<|> serveDirectoryWebApp (toString publicFolder)
        :<|> streamEmojis
        :<|> pure notFoundHandler
  where
    seed' = coerce sess ?: ColorSeed 86
    isHXRequest' = isHXRequest ?: IsHXRequest False
    notFoundHandler _ res = do
        resp <-
            runM
                $ runInputSem (embed @IO $ randomRIO @Int (minInt, maxInt))
                $ runReader seed'
                $ runInputConst isHXRequest'
                $ renderSite None lostPage
        res
            $ responseLBS status404 [("Content-Type", "text/html; charset=UTF-8")]
            $ renderMarkup resp

faviconHandler ::
    (Members '[Reader ColorSeed] r) =>
    ServerT FaviconAPI (Sem r)
faviconHandler = do
    plt <- askColorPalette
    let img = makeImage @VS @_ @Double
            (16, 16)
            \(_y, x) ->
                lerp
                    (fromIntegral x / 16)
                    (bgPixel plt)
                    (fg1Pixel plt)
    pure $ addHeader "no-store" $ encode PNG [] img
  where
    lerp f a b = f * a + (1 - f) * b
    clayToPixelDouble = fmap ((/ 255.0) . fromIntegral) . clayColorToPixelRGB
    fg1Pixel plt = clayToPixelDouble (plt ^. #fg1)
    bgPixel plt = clayToPixelDouble (plt ^. #bg)
