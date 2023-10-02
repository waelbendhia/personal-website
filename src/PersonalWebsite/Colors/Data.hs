module PersonalWebsite.Colors.Data (ColorSeed (..)) where

import Data.Binary
import qualified Data.ByteString.Base58 as B58
import Relude
import Servant

newtype ColorSeed = ColorSeed Int
    deriving (FromHttpApiData) via Int

instance ToText ColorSeed where
    toText (ColorSeed s) = decodeUtf8 $ B58.encodeBase58 B58.bitcoinAlphabet $ toStrict $ encode s

instance IsString (Maybe ColorSeed) where
    fromString s = parseAsBase58 <|> parseAsNumber
      where
        parseAsNumber = ColorSeed <$> readMaybe s
        parseAsBase58 =
            toText s
                & encodeUtf8
                & B58.decodeBase58 B58.bitcoinAlphabet
                <&> toLazy
                <&> decode
                <&> ColorSeed
