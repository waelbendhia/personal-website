{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module PersonalWebsite.Blogs.Capabilities (
    HasTags,
    HasBlogs (..),
    BlogsFromFolder (..),
    BlogSource (..),
    BlogsFromSource (..),
) where

import Capability.Reflection
import Capability.Source
import Data.List (nub)
import qualified Data.Text as T
import Data.Yaml
import Katip
import Optics
import PersonalWebsite.Blogs.Data
import Relude
import UnliftIO
import UnliftIO.Directory

data BlogSource = Folder Text | Github Text deriving (Show)

type HasTags = HasSource "tags" [Text]

class Monad m => HasBlogs m where
    getBlogs :: Int -> Maybe Text -> m [BlogEntry]
    getBlog :: Text -> m (Maybe BlogEntry)

newtype BlogsFromFolder m a = BlogsFromFolder {unBlogsFromFolder :: m a}
    deriving (Functor, Applicative, Monad, MonadIO)

stripYml :: Text -> Text
stripYml t = fromMaybe t $ T.stripSuffix ".yml" t

instance
    (Monad m, MonadIO m, HasSource "folder" Text m, KatipContext m) =>
    HasSource "tags" [Text] (BlogsFromFolder m)
    where
    await_ _ = do
        f <- BlogsFromFolder $ await @"folder"
        fs <- liftIO (listDirectory $ toString f)
        mapM (getBlog . stripYml . toText) fs
            <&> catMaybes
            <&> toListOf (traversed % #content % #tags)
            <&> join
            <&> nub
            <&> sort

instance
    (Monad m, MonadIO m, HasSource "folder" Text m, KatipContext m) =>
    HasBlogs (BlogsFromFolder m)
    where
    getBlogs p' mtag = BlogsFromFolder $ do
        f <- await @"folder"
        fs <- liftIO (listDirectory $ toString f)
        unBlogsFromFolder (mapM (getBlog . stripYml . toText) fs)
            <&> catMaybes
            <&> filter (\b -> maybe True (`elem` (b ^. #content % #tags)) mtag)
            <&> sortOn (Down . view #date)
            <&> drop (10 * p')
            <&> take 10

    getBlog f = BlogsFromFolder $ do
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

newtype BlogsFromRepo m a = BlogsFromRepo {unBlogsFromRepo :: m a}
    deriving (Functor, Applicative, Monad, MonadIO)

instance (Monad m, HasSource "repo" Text m) => HasBlogs (BlogsFromRepo m) where
    getBlogs _ _ = BlogsFromRepo $ await @"repo" *> undefined

    getBlog _ = BlogsFromRepo $ await @"repo" *> undefined

instance (Monad m, HasSource "repo" Text m) => HasSource "tags" [Text] (BlogsFromRepo m) where
    await_ _ = BlogsFromRepo $ await @"repo" *> undefined

newtype BlogsFromSource m a = BlogsFromSource (m a)
    deriving (Functor, Applicative, Monad, MonadIO)

withFolderOrRepo ::
    forall m a.
    (Monad m, KatipContext m, HasSource "blogSource" BlogSource m) =>
    (forall m'. (HasBlogs m', HasSource "tags" [Text] m') => m' a) ->
    m a
withFolderOrRepo withEither =
    await @"blogSource" >>= \case
        Folder f ->
            interpret @"folder" @'[KatipContext]
                (ReifiedSource $ pure f)
                (unBlogsFromFolder withEither)
        Github r ->
            interpret_ @"repo"
                (ReifiedSource $ pure r)
                (unBlogsFromRepo withEither)

instance
    (Monad m, HasSource "blogSource" BlogSource m, KatipContext m) =>
    HasBlogs (BlogsFromSource m)
    where
    getBlogs p' mtag = BlogsFromSource $ withFolderOrRepo (getBlogs p' mtag)

    getBlog f' = BlogsFromSource $ withFolderOrRepo (getBlog f')

instance
    (Monad m, HasSource "blogSource" BlogSource m, KatipContext m) =>
    HasSource "tags" [Text] (BlogsFromSource m)
    where
    await_ _ = BlogsFromSource $ withFolderOrRepo (await @"tags")
