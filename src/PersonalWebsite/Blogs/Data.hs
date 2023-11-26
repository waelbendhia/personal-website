module PersonalWebsite.Blogs.Data (
    BlogEntry (..),
    BlogData (..),
    Tags (..),
) where

import Data.Aeson
import Data.Time
import Optics
import Relude

newtype Tags = Tags [Text]

data BlogData = BlogData {title :: !Text, tags :: ![Text], content :: ![Text], date :: !Day}
    deriving (Show, Generic)

instance FromJSON BlogData

makeFieldLabelsWith noPrefixFieldLabels 'BlogData

data BlogEntry = BlogEntry
    { content :: !BlogData
    , path :: !Text
    , editDate :: !UTCTime
    }
    deriving (Show)

makeFieldLabelsWith noPrefixFieldLabels 'BlogEntry
