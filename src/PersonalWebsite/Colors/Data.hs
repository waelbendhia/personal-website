module PersonalWebsite.Colors.Data (ColorSeed (..)) where

import Relude
import Servant

newtype ColorSeed = ColorSeed Int
    deriving (FromHttpApiData) via Int

instance ToText ColorSeed where
    toText (ColorSeed s) = show s

instance IsString (Maybe ColorSeed) where
    fromString s = parseAsNumber
      where
        parseAsNumber = ColorSeed <$> readMaybe s
