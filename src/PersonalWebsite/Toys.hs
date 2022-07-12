module PersonalWebsite.Toys (
    toysHandler,
    module PersonalWebsite.Toys.PaletteGenerator,
) where

import Capability.Reader
import qualified Clay as C
import Optics ((^.))
import PersonalWebsite.API
import PersonalWebsite.Colors
import PersonalWebsite.Internal
import PersonalWebsite.Pages
import PersonalWebsite.Toys.API
import PersonalWebsite.Toys.PaletteGenerator
import Relude hiding (ask, div)
import Servant
import Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A

data Toys = PaletteGenerator deriving (Bounded, Enum)

instance ToMarkup Toys where
    toMarkup t = do
        a ! A.class_ "single-toy" ! A.href (fromLink link') $
            div $ do
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

mkToysStyle :: (HasReader "colorSeed" Int m) => m C.Css
mkToysStyle =
    askColorPalette <&> \plt ->
        C.a C.? ".single-toy" C.& do
            C.display C.block
            C.maxWidth (C.px 768)
            sequence_ $ [C.marginLeft, C.marginRight] ?? C.auto
            sequence_ $ [C.paddingRight, C.paddingLeft] ?? C.px 16
            C.transition "all" (C.ms 150) C.ease (C.ms 150)
            C.border (C.px 2) C.solid (plt ^. #fg2)
            C.color (plt ^. #fg1)
            C.background $ setTransparency 0.4 (plt ^. #fg2)
            ":hover" C.& do
                C.color (plt ^. #fg1)
                C.borderColor (plt ^. #primary)
                C.background $ setTransparency 0.4 (plt ^. #primary)

mkToysPage :: (HasReader "colorSeed" Int m) => m Html
mkToysPage =
    mkToysStyle <&> \toysStyle -> do
        style $ toMarkup $ C.render toysStyle
        p $ do
            "Toys are various little doodads that do things. "
            "Some of these might have blog posts going more in depth into how they work."
        div ! A.class_ "toys-container" $ mapM_ toMarkup (universe @Toys)

colorGeneratorHandler :: (HasReader "colorSeed" Int m) => ServerT ColorGeneratorAPI m
colorGeneratorHandler = renderSite Toys <=< colorGeneratorPage

toysHandler :: (HasReader "colorSeed" Int m) => ServerT ToysAPI m
toysHandler = colorGeneratorHandler :<|> (renderSite Toys =<< mkToysPage)
