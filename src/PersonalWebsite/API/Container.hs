module PersonalWebsite.API.Container (Container (..), Tab (..)) where

import qualified Clay as C
import PersonalWebsite.API.CSS
import PersonalWebsite.Nord
import Relude hiding (div, head, (**))
import Skylighting
import Text.Blaze
import qualified Text.Blaze.Html4.FrameSet.Attributes as A
import Text.Blaze.Html5

data Tab = Home | Blog | None deriving (Eq)

data Container a = Container
    { activeTab :: !Tab
    , content :: !a
    }

instance ToMarkup a => ToMarkup (Container a) where
    toMarkup (Container at cnt) =
        html $ do
            head $ do
                title "Wael's very own super special personal website"
                style $ toMarkup $ C.render (baseStyle darkPalette)
                style $ toMarkup $ styleToCss nord
                link ! A.rel "stylesheet" ! A.href "https://cdn.jsdelivr.net/gh/aymanbagabas/iosevka-fonts@v6.1.2/dist/iosevka/iosevka.min.css"
                link ! A.rel "stylesheet" ! A.href "https://cdnjs.cloudflare.com/ajax/libs/Iosevka/6.0.0/iosevka-etoile/iosevka-etoile.min.css"
            body $ do
                header $ do
                    h2 "header"
                    div ! A.class_ "nav" $ do
                        a ! A.href "/" ! A.class_ (if at == Home then "active" else "") $ "home"
                        a ! A.href "/blog" ! A.class_ (if at == Blog then "active" else "") $ "blog"
                div ! A.class_ "content" $ do
                    toMarkup cnt
