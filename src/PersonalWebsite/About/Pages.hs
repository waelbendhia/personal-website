module PersonalWebsite.About.Pages (aboutPage) where

import Data.Aeson.Optics
import Data.FileEmbed
import Data.Yaml
import Optics
import Polysemy
import Relude hiding (div)
import Text.Blaze.XHtml5

eCV :: Either ParseException Value
eCV = decodeEither' @Value $(embedFile "./docs/cv.yaml")

aboutPage :: Sem r Html
aboutPage = pure do
    eCV & either
        (\_ -> div "This isn't real.")
        \cv -> do
            div do
                h2 "Summary"
                mapM_
                    (mapM_ (p . fromString . toString) . lines)
                    (cv ^? key "generalInfo" % key "about" % key "long" % _String)
                h2 "Stuff I can do"
