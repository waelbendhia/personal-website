module PersonalWebsite.Blogs.Pages (
    toSummary,
    blogsPage,
    renderBlogEntry,
    tagsPage,
) where

import Data.Text hiding (span)
import qualified Data.Text as T hiding (span)
import Data.Time
import Optics hiding (pre)
import PersonalWebsite.API
import PersonalWebsite.Blogs.API
import PersonalWebsite.Blogs.Capabilities
import PersonalWebsite.Blogs.Data
import PersonalWebsite.Internal
import PersonalWebsite.Pandoc
import Polysemy
import Polysemy.Input
import Relude hiding (div, span)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.XHtml5

truncateMD :: Int -> [Text] -> [Text]
truncateMD maxLines = go 0
  where
    takeNLines n t = T.unlines $ Relude.take n $ T.lines t
    go _ [] = []
    go acc (l : ls) =
        let numLines = T.count "\n" l
         in if numLines + acc > maxLines
                then -- Why add three ticks at the end? Well in case we truncate a fenced code block.
                -- It isn't dumb if it works
                    [takeNLines (maxLines - acc) l <> "\n```\n"]
                else l : go (acc + numLines) ls

blogsLink :: Maybe Int -> Maybe Text -> Attribute
blogsLink p' t =
    A.href (fromLink $ apiLink (Proxy @PageBlogsAPI) p' t)

toSummary :: Member Render r => BlogEntry -> Sem r Html
toSummary be =
    renderBlogEntry (be & #content % #content %~ truncateMD 13) <&> \body' ->
        div ! A.class_ "blog-item" $ do
            div ! A.class_ "summary" $ body'
            div ! A.class_ "link-row" $
                a ! A.href (fromLink $ apiLink (Proxy @BlogAPI) (be ^. #path)) $
                    "GOTO"

renderTag :: Text -> Html
renderTag t = a ! blogsLink Nothing (Just t) ! A.class_ "tag" $ toMarkup t

renderBlogEntry :: Member Render r => BlogEntry -> Sem r Html
renderBlogEntry be =
    mapM renderMarkdown (be ^. #content % #content) <&> \body' -> do
        h2 $ toMarkup $ be ^. #content % #title
        div ! A.class_ "metadata" $ do
            div $ do
                toMarkup $ formatTime defaultTimeLocale "%B %e %Y" (be ^. #content % #date)
                br
                "Last modified on "
                toMarkup $ formatTime defaultTimeLocale "%B %e %Y" (be ^. #editDate)
            div ! A.class_ "tags" $ mapM_ renderTag (be ^. #content % #tags)
        sequence_ body'

pager :: (Foldable t) => Int -> t a -> Html
pager p' bs = div ! A.class_ "pager" $ do
    condA (p' == 0) pred "back"
    div $ "page " <> show (p' + 1)
    condA (Relude.length bs < 10) succ "forward"
  where
    condA cond d = a !? (cond, A.class_ "disabled") ! blogsLink (Just $ d p') Nothing

renderTagHeader :: Text -> Html
renderTagHeader t = div ! A.class_ "tag-header" $ do
    h1 $ "Posts about " *> (span ! A.class_ "tag") (toMarkup t) *> "."
    p $ do
        "View "
        a ! blogsLink Nothing Nothing $ "all posts"
        "."
        br
        "See "
        a ! A.href (fromLink $ apiLink (Proxy @TagsAPI)) $ "all tags"
        "."

blogsPage ::
    Members '[Blogs, Render] r =>
    Int ->
    Maybe Text ->
    Sem r Html
blogsPage p' mtag = do
    bs <- getBlogs p' mtag
    summaries <- mapM toSummary bs
    pure $
        div ! A.class_ "blog-list" $ do
            pager p' bs
            mapM_ renderTagHeader mtag
            sequence_ . Relude.intersperse (div ! A.class_ "seperator" $ pass) $ summaries
            when (Relude.null bs) $ div ! A.class_ "empty" $ "Nothing to see here."
            pager p' bs

tagsPage :: (Members '[Input Tags] r) => Sem r Html
tagsPage = inputs $ \ts -> do
    h2 "Here's all the tags"
    div ! A.class_ "tags" $ coerce @Tags @[Text] ts `forM_` renderTag
