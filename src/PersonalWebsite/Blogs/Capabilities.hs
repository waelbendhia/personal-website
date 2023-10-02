{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module PersonalWebsite.Blogs.Capabilities (
    Blogs (..),
    getBlogs,
    getBlog,
    runTagsFromFolder,
    runBlogsFromFolder,
    traceBlogs,
) where

import Data.List (nub)
import qualified Data.Text as T
import Data.Time
import Data.Yaml
import qualified GHC.IO as CE
import Katip
import qualified OpenTelemetry.Trace as Otel
import Optics
import PersonalWebsite.Blogs.Data
import PersonalWebsite.Katip
import PersonalWebsite.Tracing
import Polysemy
import Polysemy.Input
import Polysemy.KVStore
import Relude
import System.Directory

data Blogs m a where
    GetBlogs :: Int -> Maybe Text -> Blogs m [BlogEntry]
    GetBlog :: Text -> Blogs m (Maybe BlogEntry)

makeSem ''Blogs

stripYml :: Text -> Text
stripYml t = fromMaybe t $ T.stripSuffix ".yml" t

traceBlogs :: Members [Blogs, Tracing] r => Sem r a -> Sem r a
traceBlogs = intercept \case
    GetBlogs p' mtag ->
        inSpan'
            "GetBlogs"
            Otel.defaultSpanArguments
                { Otel.attributes =
                    fromList
                        [ ("page", Otel.toAttribute p')
                        , ("tag", Otel.toAttribute $ fromMaybe "No tag" mtag)
                        ]
                }
            $ getBlogs p' mtag
    GetBlog f ->
        inSpan'
            "GetBlog"
            Otel.defaultSpanArguments{Otel.attributes = one ("blog-name", Otel.toAttribute f)}
            $ getBlog f

getBlogIO ::
    Members '[Embed IO, KVStore Text (UTCTime, BlogEntry), Katiper, Tracing] r =>
    Text ->
    Text ->
    Sem r (Maybe BlogEntry)
getBlogIO folder fn = katipAddContext (sl "file-path" fp) $ do
    mblog <- lookupKV fp
    case mblog of
        Nothing -> inSpan' "readBlogFromFile" Otel.defaultSpanArguments readBlogFromFile
        Just (ts, be) -> do
            d <- embed $ getModificationTime (toString fp)
            if ts >= d
                then do
                    $logTM DebugS "cache hit!"
                    pure $ Just be
                else readBlogFromFile
  where
    fp = folder <> "/" <> fn <> ".yml"
    readBlogFromFile = do
        $logTM DebugS "cache miss!"
        ecnt <-
            embed @IO $
                CE.catch @SomeException
                    (Right <$> readFileText (toString fp))
                    (pure . Left)
        case ecnt of
            Left ex -> katipAddContext (sl "error" (show @Text ex)) $ do
                $logTM ErrorS "could not get blog"
                pure Nothing
            Right cnt -> katipAddContext (sl "file-path" fp) $ do
                $logTM DebugS "read file"
                case decodeEither' (encodeUtf8 cnt) of
                    Left e -> katipAddContext (sl "error" (show @Text e)) $ do
                        $logTM ErrorS "decoding failed"
                        pure Nothing
                    Right x -> do
                        d <- embed $ getModificationTime (toString fp)
                        let be = Just $ BlogEntry{content = x, path = fn, editDate = d}
                        updateKV fp ((d,) <$> be)
                        pure be

getBlogsIO ::
    Members '[Embed IO, KVStore Text (UTCTime, BlogEntry), Tracing, Katiper] r =>
    Text ->
    Sem r [BlogEntry]
getBlogsIO folder = do
    fs <-
        inSpan' "listDirectory" Otel.defaultSpanArguments . embed . listDirectory $
            toString folder
    catMaybes <$> mapM (runBlogsFromFolder folder . getBlog . stripYml . toText) fs

runTagsFromFolder ::
    Members '[Embed IO, KVStore Text (UTCTime, BlogEntry), Tracing, Katiper] r =>
    Text ->
    Sem (Input Tags ': r) a ->
    Sem r a
runTagsFromFolder folder = interpret \case
    Input ->
        getBlogsIO folder
            <&> toListOf (traversed % #content % #tags)
            <&> join
            <&> nub
            <&> sort
            <&> Tags

runBlogsFromFolder ::
    forall r a.
    Members '[Embed IO, KVStore Text (UTCTime, BlogEntry), Tracing, Katiper] r =>
    Text ->
    Sem (Blogs : r) a ->
    Sem r a
runBlogsFromFolder folder = interpret \case
    GetBlogs p' mtag -> do
        getBlogsIO folder
            <&> filter (\b -> maybe True (`elem` (b ^. #content % #tags)) mtag)
            <&> sortOn (Down . view (#content % #date))
            <&> drop (10 * p')
            <&> take 10
    GetBlog f -> getBlogIO folder f
