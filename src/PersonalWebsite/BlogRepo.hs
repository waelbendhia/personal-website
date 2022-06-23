{-# LANGUAGE TemplateHaskell #-}

module PersonalWebsite.BlogRepo (
    HasBlogRepo (..),
    BlogRepoFromFolder (..),
) where

import Capability.Source
import Control.Monad
import qualified Data.Text as T
import Data.Time
import Data.Yaml
import Katip
import PersonalWebsite.Markdown
import Relude
import UnliftIO.Directory (getModificationTime, listDirectory)

class Monad m => HasBlogRepo m where
    getBlogs :: Int -> m [BlogEntry]
    getBlog :: Text -> m (Maybe BlogEntry)

newtype BlogRepoFromFolder m a = BlogRepoFromFolder (m a)
    deriving (Functor, Applicative, Monad, MonadIO)

unBlogRepoFromFolder :: BlogRepoFromFolder m a -> m a
unBlogRepoFromFolder (BlogRepoFromFolder m) = m

stripYml :: Text -> Text
stripYml t = fromMaybe t $ T.stripSuffix ".yml" t

instance
    (Monad m, MonadIO m, HasSource "folder" Text m, KatipContext m) =>
    HasBlogRepo (BlogRepoFromFolder m)
    where
    getBlogs _ = BlogRepoFromFolder $ do
        f <- await @"folder"
        fs <- liftIO $ listDirectory $ toString f
        catMaybes <$> unBlogRepoFromFolder (mapM (getBlog . stripYml . toText) fs)
    getBlog f = BlogRepoFromFolder $ do
        fp <- awaits @"folder" $ \folder -> folder <> "/" <> f <> ".yml"
        cnt <- readFileText $ toString fp
        katipAddContext (sl "file-content" cnt) $ do
            $(logTM) DebugS "read file"
            case decodeEither' $ encodeUtf8 cnt of
                Left e -> katipAddContext (sl "error" (show @Text e)) $ do
                    $(logTM) ErrorS "decoding failed"
                    pure Nothing
                Right x -> do
                    d <- utctDay <$> getModificationTime (toString fp)
                    pure $ Just $ BlogEntry{content = x, path = f, time = d}
