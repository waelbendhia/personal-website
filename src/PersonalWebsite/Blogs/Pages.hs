module PersonalWebsite.Blogs.Pages (
    toSummary,
    blogsPage,
    renderBlogEntry,
    tagsPage,
) where

import Data.Text hiding (elem, find, span)
import qualified Data.Text as T hiding (elem, find, span)
import Data.Time
import Optics hiding (Empty, pre)
import PersonalWebsite.API
import PersonalWebsite.Blogs.API
import PersonalWebsite.Blogs.Capabilities
import PersonalWebsite.Blogs.Data
import PersonalWebsite.HTMX
import PersonalWebsite.Internal
import PersonalWebsite.Pandoc
import Polysemy
import Polysemy.Input
import Relude hiding (div, span)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal
import Text.Blaze.Renderer.String
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

blogsLink :: Maybe Int -> Maybe Text -> AttributeValue
blogsLink p' t = fromLink $ apiLink (Proxy @PageBlogsAPI) p' t

toSummary :: (Member Render r) => BlogEntry -> Sem r Html
toSummary be =
    renderBlogEntry False (be & #content % #content %~ truncateMD 13) <&> \body' ->
        div ! A.class_ "blog-item" $ do
            div ! A.class_ "summary" $ body'
            div
                ! A.class_ "link-row"
                $ hxA
                    (fromLink $ apiLink (Proxy @BlogAPI) (be ^. #path))
                    "GOTO"

renderTag :: Text -> Html
renderTag t = hxA (blogsLink Nothing (Just t)) ! A.class_ "tag" $ toMarkup t

getHeadingLevel :: StaticString -> Maybe Int
getHeadingLevel t = snd <$> find (\(h, _) -> h == tAsText) headings
  where
    tAsText = getText t
    headings = [1 .. 6] <&> \n -> ("h" <> show n, n)

extractText :: MarkupM a -> Text
extractText (Parent _ _ _ ns) = extractText ns
extractText (CustomParent _ ns) = extractText ns
extractText (Content s _) = toText $ fromChoiceString s ""
extractText (Append x y) = extractText x <> extractText y
extractText (AddAttribute _ _ _ ns) = extractText ns
extractText (AddCustomAttribute _ _ ns) = extractText ns
extractText _ = ""

getHeadings :: MarkupM a -> [(Int, Text, Text)]
getHeadings = go Nothing
  where
    go :: Maybe Text -> MarkupM a -> [(Int, Text, Text)]
    go (Just elemID) (Parent t _ _ ns) =
        fromMaybe
            (go (Just elemID) ns)
            (getHeadingLevel t <&> \lvl -> [(lvl, extractText ns, elemID)])
    go Nothing (Parent _ _ _ ns) = go Nothing ns
    -- Maybe check this as well
    go lastID (CustomParent _ ns) = go lastID ns
    go lastID (Append x y) = go lastID x <> go lastID y
    go lastID (AddAttribute rk k val ns) =
        go
            ( if getText rk == "id" || getText k == "id"
                then Just (toText $ fromChoiceString val "")
                else lastID
            )
            ns
    go lastID (AddCustomAttribute _ _ ns) = go lastID ns
    go _ _ = []

renderTOC :: [Html] -> Html
renderTOC doc = ul ! A.class_ "toc" $ do
    h3 "Contents"
    forM_ (getHeadings =<< doc) \(lvl, t, elemID) ->
        li
            ! A.class_ ("level-" <> show lvl)
            $ a
            ! A.href (fromText $ "#" <> elemID)
            $ text t

renderBlogEntry :: (Member Render r) => Bool -> BlogEntry -> Sem r Html
renderBlogEntry withTOC be =
    mapM renderMarkdown (be ^. #content % #content) <&> \body' -> do
        h2 $ toMarkup $ be ^. #content % #title
        div ! A.class_ "metadata" $ do
            div do
                toMarkup $ formatTime defaultTimeLocale "%B %e %Y" (be ^. #content % #date)
                br
                "Updated on "
                toMarkup $ formatTime defaultTimeLocale "%B %e %Y" (be ^. #editDate)
            div ! A.class_ "tags" $ mapM_ renderTag (be ^. #content % #tags)
        when withTOC $ renderTOC body'
        sequence_ body'

pager :: (Foldable t) => Int -> t a -> Html
pager p' bs = div ! A.class_ "pager" $ do
    condA (p' == 0) pred "back"
    div $ "page " <> show (p' + 1)
    condA (Relude.length bs < 10) succ "forward"
  where
    condA cond d = hxA (blogsLink (Just $ d p') Nothing) !? (cond, A.class_ "disabled")

renderTagHeader :: Text -> Html
renderTagHeader t = div ! A.class_ "tag-header" $ do
    h1 $ "Posts about " *> (span ! A.class_ "tag") (toMarkup t) *> "."
    p do
        "View "
        hxA (blogsLink Nothing Nothing) "all posts"
        "."
        br
        "See "
        hxA (fromLink $ apiLink (Proxy @TagsAPI)) "all tags"
        "."

blogsPage ::
    (Members '[Blogs, Render] r) =>
    Int ->
    Maybe Text ->
    Sem r Html
blogsPage p' mtag = do
    bs <- getBlogs p' mtag
    summaries <- mapM toSummary bs
    pure
        $ div
        ! A.class_ "blog-list"
        $ do
            pager p' bs
            mapM_ renderTagHeader mtag
            sequence_ . Relude.intersperse (div ! A.class_ "seperator" $ pass) $ summaries
            when (Relude.null bs) $ div ! A.class_ "empty" $ "Nothing to see here."
            pager p' bs

tagsPage :: (Members '[Input Tags] r) => Sem r Html
tagsPage = inputs $ \ts -> do
    h2 "Here's all the tags"
    div ! A.class_ "tags" $ coerce @Tags @[Text] ts `forM_` renderTag
