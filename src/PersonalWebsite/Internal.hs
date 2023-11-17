module PersonalWebsite.Internal (
    scopeCSS,
    marginRem,
    paddingRem,
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

on4 :: (b -> b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> a -> c
on4 op f x y z a = op (f x) (f y) (f z) (f a)

marginRem :: Double -> Double -> Double -> Double -> C.Css
marginRem = C.margin `on4` C.rem

paddingRem :: Double -> Double -> Double -> Double -> C.Css
paddingRem = C.padding `on4` C.rem

fromText :: (IsString s) => Text -> s
fromText = fromString . toString

fromLink :: (IsString s) => Servant.API.Link -> s
fromLink = fromText . ("/" <>) . toUrlPiece
