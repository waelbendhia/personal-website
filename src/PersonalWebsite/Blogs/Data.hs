module PersonalWebsite.Blogs.Data (BlogEntry (..), BlogData (..)) where

import Data.Aeson
import Data.Time
import Optics
import Relude

data BlogData = BlogData {title :: !Text, tags :: ![Text], content :: ![Text]}
    deriving (Show, Generic)

instance FromJSON BlogData

makeFieldLabelsWith noPrefixFieldLabels 'BlogData

data BlogEntry = BlogEntry {content :: !BlogData, path :: !Text, date :: !UTCTime}
    deriving (Show)

makeFieldLabelsWith noPrefixFieldLabels 'BlogEntry
