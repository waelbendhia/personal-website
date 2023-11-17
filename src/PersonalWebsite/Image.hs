module PersonalWebsite.Image (ICO) where

import Relude
import Servant

data ICO deriving (Typeable)

instance Accept ICO where
    contentType _ = "image/x-icon"

instance MimeRender ICO ByteString where
    mimeRender _ = fromStrict
