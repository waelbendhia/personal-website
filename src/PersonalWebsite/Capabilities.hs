{-# LANGUAGE TemplateHaskell #-}

module PersonalWebsite.Capabilities (HasTags, HasBlogRepo (..), BlogRepoFromFolder (..)) where

import Capability.Source
import Data.List (nub)
import qualified Data.Text as T
import Data.Yaml
import Katip
import Optics
import PersonalWebsite.Blog
import Relude
import UnliftIO
import UnliftIO.Directory

type HasTags = HasSource "tags" [Text]

class Monad m => HasBlogRepo m where
    getBlogs :: Int -> Maybe Text -> m [BlogEntry]
    getBlog :: Text -> m (Maybe BlogEntry)

newtype BlogRepoFromFolder m a = BlogRepoFromFolder (m a)
    deriving (Functor, Applicative, Monad, MonadIO)

unBlogRepoFromFolder :: BlogRepoFromFolder m a -> m a
unBlogRepoFromFolder (BlogRepoFromFolder m) = m

stripYml :: Text -> Text
stripYml t = fromMaybe t $ T.stripSuffix ".yml" t

instance
    (Monad m, MonadIO m, HasSource "folder" Text m, KatipContext m) =>
    HasSource "tags" [Text] (BlogRepoFromFolder m)
    where
    await_ _ = BlogRepoFromFolder $ do
        f <- await @"folder"
        fs <- liftIO (listDirectory $ toString f)
        unBlogRepoFromFolder (mapM (getBlog . stripYml . toText) fs)
            <&> catMaybes
            <&> toListOf (traversed % #content % #tags)
            <&> nub
            <&> join
            <&> sort

instance
    (Monad m, MonadIO m, HasSource "folder" Text m, KatipContext m) =>
    HasBlogRepo (BlogRepoFromFolder m)
    where
    getBlogs p' mtag = BlogRepoFromFolder $ do
        f <- await @"folder"
        fs <- liftIO (listDirectory $ toString f)
        unBlogRepoFromFolder (mapM (getBlog . stripYml . toText) fs)
            <&> catMaybes
            <&> filter (\b -> maybe True (`elem` (b ^. #content % #tags)) mtag)
            <&> sortOn (Down . view #date)
            <&> drop (10 * p')
            <&> take 10

    getBlog f = BlogRepoFromFolder $ do
        fp <- awaits @"folder" $ \folder -> folder <> "/" <> f <> ".yml"
        mcnt <- liftIO . catchAny (Just <$> readFileText (toString fp)) $ const (pure Nothing)
        fmap join . forM mcnt $ \cnt -> katipAddContext (sl "file-content" cnt) $ do
            $logTM DebugS "read file"
            case decodeEither' (encodeUtf8 cnt) of
                Left e -> katipAddContext (sl "error" (show @Text e)) $ do
                    $logTM ErrorS "decoding failed"
                    pure Nothing
                Right x -> do
                    d <- getModificationTime (toString fp)
                    pure $ Just $ BlogEntry{content = x, path = f, date = d}
