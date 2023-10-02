module PersonalWebsite.Internal (
    scopeCSS,
    marginPx,
    paddingPx,
    on3,
    fromText,
    fromLink,
) where

import qualified Clay as C
import Data.Attoparsec.Text
import qualified Data.Text.Lazy.Builder as B
import Optics
import PersonalWebsite.CSS.Parser
import Relude
import Relude.Extra hiding ((%~))
import Servant (ToHttpApiData (toUrlPiece))
import qualified Servant as Servant.API

mapSelector :: (Text -> Text) -> [Block] -> [Block]
mapSelector f = bimapF mapMediaQuery mapCSSBlock
  where
    mapCSSBlock = #selectors %~ (coerced @_ @(NonEmpty Text) %~ fmap f)
    mapMediaQuery = #rules % mapped %~ mapCSSBlock

scopeCSS :: Text -> Text -> Text
scopeCSS superClass css' =
    parseOnly css css'
        & either
            (const css')
            (toStrict . B.toLazyText . renderCSS . mapSelector ((superClass <> " ") <>))

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

marginPx :: Double -> Double -> Double -> C.Size C.LengthUnit -> C.Css
marginPx = C.margin `on3` C.px

paddingPx :: Double -> Double -> Double -> C.Size C.LengthUnit -> C.Css
paddingPx = C.padding `on3` C.px

fromText :: IsString s => Text -> s
fromText = fromString . toString

fromLink :: IsString s => Servant.API.Link -> s
fromLink = fromText . ("/" <>) . toUrlPiece
