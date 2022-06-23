module PersonalWebsite.Markdown (
    BlogData (..),
    BlogEntry (..),
    toSummary,
    renderPage,
    renderBlogEntry,
) where

import Data.Aeson
import Data.Text
import Data.Time
import Optics hiding (pre)
import Relude hiding (div)
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

data BlogData = BlogData
    { title :: !Text
    , content :: ![Text]
    }
    deriving (Show, Generic)

instance FromJSON BlogData

makeFieldLabelsWith noPrefixFieldLabels 'BlogData

data BlogEntry = BlogEntry
    { content :: !BlogData
    , path :: !Text
    , time :: !Day
    }
    deriving (Show)

makeFieldLabelsWith noPrefixFieldLabels 'BlogEntry

toSummary :: PandocMonad m => BlogEntry -> m Html
toSummary be = do
    body' <- mapM blockToHTML (be ^. #content % #content)
    pure . (div ! A.class_ "blog-item") $ do
        h1 $ toMarkup $ be ^. #content % #title
        div ! A.class_ "summary" $ sequence_ body'
        div ! A.class_ "link-row" $
            a ! A.href (fromString $ toString $ "/blog/" <> be ^. #path) $ "GOTO"

renderBlogEntry :: PandocMonad m => BlogEntry -> m Html
renderBlogEntry be = do
    body' <- mapM blockToHTML (be ^. #content % #content)
    pure $ do
        h1 $ toMarkup $ be ^. #content % #title
        sequence_ body'

renderPage :: PandocMonad m => Int -> [BlogEntry] -> m Html
renderPage _ bs = do
    summaries <- mapM toSummary bs
    pure
        . (div ! A.class_ "blog-list")
        . sequence_
        . Relude.intersperse (div ! A.class_ "seperator" $ pass)
        $ summaries
