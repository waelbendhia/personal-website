module PersonalWebsite.Toys (
    toysHandler,
    module PersonalWebsite.Toys.PaletteGenerator,
) where

import qualified Clay as C
import Optics ((^.))
import PersonalWebsite.API
import PersonalWebsite.Colors
import PersonalWebsite.HTMX
import PersonalWebsite.Internal
import PersonalWebsite.LiveReload
import PersonalWebsite.Pages
import PersonalWebsite.Pandoc
import PersonalWebsite.Toys.API
import PersonalWebsite.Toys.PaletteGenerator
import Polysemy
import Polysemy.Input
import Polysemy.Reader
import Relude hiding (Reader, ask, div)
import Servant
import Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A

data Toys = PaletteGenerator deriving (Bounded, Enum)

instance ToMarkup Toys where
    toMarkup t = hxA (fromLink link') ! A.class_ "single-toy" $ div do
        h1 title'
        p description
      where
        (title', description, link') =
            case t of
                PaletteGenerator ->
                    ( "Randomized color palette"
                    , "Generate random, mildly acceptable, color palettes"
                    , apiLink (Proxy @ColorGeneratorAPI) Nothing
                    )

mkToysStyle :: (Member (Reader ColorSeed) r) => Sem r C.Css
mkToysStyle =
    askColorPalette <&> \plt ->
        C.a C.? ".single-toy" C.& do
            C.display C.block
            C.maxWidth (C.px 768)
            sequence_ $ [C.marginLeft, C.marginRight] ?? C.auto
            sequence_ $ [C.paddingRight, C.paddingLeft] ?? C.rem 1
            C.transition "all" (C.ms 150) C.ease (C.ms 150)
            C.border (C.px 2) C.solid (plt ^. #fg2)
            C.color (plt ^. #fg1)
            C.background $ setTransparency 0.4 (plt ^. #fg2)
            ":hover" C.& do
                C.color (plt ^. #fg1)
                C.borderColor (plt ^. #primary)
                C.background $ setTransparency 0.4 (plt ^. #primary)

mkToysPage :: (Member (Reader ColorSeed) r) => Sem r Html
mkToysPage =
    mkToysStyle <&> \toysStyle -> do
        style $ toMarkup $ C.renderWith C.compact [] toysStyle
        p do
            "Toys are various little doodads that do things. "
            "Some of these might have blog posts going more in depth into how they work."
        div ! A.class_ "toys-container" $ mapM_ toMarkup (universe @Toys)

colorGeneratorHandler ::
    ( Members
        [ Reader ColorSeed
        , Render
        , Input Int
        , Input IsHXRequest
        , Input UseLiveReload
        ]
        r
    ) =>
    ServerT ColorGeneratorAPI (Sem r)
colorGeneratorHandler = renderSite None <=< colorGeneratorPage

toysHandler ::
    ( Members
        [ Reader ColorSeed
        , Render
        , Input Int
        , Input IsHXRequest
        , Input UseLiveReload
        ]
        r
    ) =>
    ServerT ToysAPI (Sem r)
toysHandler = colorGeneratorHandler :<|> (renderSite None =<< mkToysPage)
