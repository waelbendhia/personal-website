module PersonalWebsite.Pages.Blog (
    BlogData (..),
    BlogEntry (..),
    toSummary,
    blogsPage,
    renderBlogEntry,
    lost,
    tagsPage,
) where

import Capability.Source
import Data.Text hiding (span)
import qualified Data.Text as T hiding (span)
import Data.Time
import Optics hiding (pre)
import PersonalWebsite.Blog
import PersonalWebsite.Capabilities
import Relude hiding (div, span)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.XHtml5
import Text.Pandoc
import Text.Pandoc.Highlighting

blockToHTML :: PandocMonad m => Text -> m Html
blockToHTML t = do
    md <-
        readMarkdown
            def
                { readerExtensions =
                    extensionsFromList
                        [ Ext_backtick_code_blocks
                        , Ext_fenced_code_attributes
                        , Ext_fenced_code_blocks
                        ]
                }
            t
    writeHtml5 def{writerHighlightStyle = Just zenburn} md

truncateMD :: Int -> [Text] -> [Text]
truncateMD maxLines = go 0
  where
    takeNLines n t = T.unlines $ Relude.take n $ T.lines t
    go _ [] = []
    go acc (l : ls) =
        if numLines + acc > maxLines
            then -- Why add three ticks at the end? Well in case we truncate a fenced code block.
            -- It isn't dumb if it works
                [takeNLines (maxLines - acc) l <> "\n```\n"]
            else l : go (acc + numLines) ls
      where
        numLines = T.count "\n" l

toSummary :: PandocMonad m => BlogEntry -> m Html
toSummary be = do
    body' <- renderBlogEntry (be & #content % #content %~ truncateMD 13)
    pure . (div ! A.class_ "blog-item") $ do
        div ! A.class_ "summary" $ body'
        div ! A.class_ "link-row" $
            a ! A.href (fromString $ toString $ "/blog/" <> be ^. #path) $ "GOTO"

renderTag :: Text -> Html
renderTag t =
    a ! A.href (fromString $ toString $ "/blog?tag=" <> t) ! A.class_ "tag" $ toMarkup t

renderBlogEntry :: PandocMonad m => BlogEntry -> m Html
renderBlogEntry be = do
    body' <- mapM blockToHTML (be ^. #content % #content)
    pure $ do
        h2 $ toMarkup $ be ^. #content % #title
        div ! A.class_ "metadata" $ do
            div $ toMarkup $ formatTime defaultTimeLocale "%B %e %Y" (be ^. #date)
            div ! A.class_ "tags" $ mapM_ renderTag (be ^. #content % #tags)
        sequence_ body'

pager :: (Foldable t) => Int -> t a -> Html
pager p' bs = div ! A.class_ "pager" $ do
    condA (p' == 0) ! pageRef (p' - 1) $ "back"
    div $ "page " <> show (p' + 1)
    condA (Relude.length bs < 10) ! pageRef (p' - 1) $ "forward"
  where
    pageRef t = A.href ("/blog?page=" <> show t)
    condA cond =
        if cond then a ! A.class_ "disabled" else a

renderTagHeader :: Text -> Html
renderTagHeader t =
    div ! A.class_ "tag-header" $ do
        h1 $ do
            "Posts about "
            span ! A.class_ "tag" $ toMarkup t
            "."
        p $ do
            "View "
            a ! A.href "/blog" $ "all posts"
            "."
            br
            "See "
            a ! A.href "/blog/tags" $ "all tags"
            "."

blogsPage ::
    (HasBlogRepo m, PandocMonad m) =>
    Int ->
    Maybe Text ->
    m Html
blogsPage p' mtag = do
    bs <- getBlogs p' mtag
    summaries <- mapM toSummary bs
    pure . (div ! A.class_ "blog-list") $ do
        pager p' bs
        mapM_ renderTagHeader mtag
        sequence_ . Relude.intersperse (div ! A.class_ "seperator" $ pass) $ summaries
        when (Relude.null bs) $ div ! A.class_ "empty" $ "Nothing to see here."
        pager p' bs

lost :: Html
lost = (div ! A.class_ "lost") . p $ do
    span "You seem lost, why don't you navigate your way back "
    a ! A.href "/" $ "home"
    span "."

tagsPage :: HasTags m => m Html
tagsPage = do
    ts <- await @"tags"
    pure $ do
        h2 "Here's all the tags"
        div ! A.class_ "tags" $ mapM_ renderTag (ts <> ts <> ts <> ts <> ts)
