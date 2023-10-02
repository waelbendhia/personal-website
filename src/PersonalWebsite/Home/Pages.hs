module PersonalWebsite.Home.Pages (homePage) where

import qualified Data.Text as T
import PersonalWebsite.API
import PersonalWebsite.Blogs (BlogEntry, toSummary)
import PersonalWebsite.Blogs.API
import PersonalWebsite.Internal
import PersonalWebsite.Pandoc
import Polysemy
import Relude hiding (div, span)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal
import Text.Blaze.XHtml5

introBlock :: Html
introBlock = div ! A.class_ "intro-block" $ do
    p "Hello. I'm Wael. This is my personal website. "
    p $ do
        "I'll be posting things here about various topics. "
        "I write mostly about programming, mainly about "
        tagLink "Haskell"
        " or "
        tagLink "Go"
        " and maybe some "
        tagLink "Javascript"
        "/"
        tagLink "Typescript"
        "."
  where
    tagLink t =
        a
            ! A.href
                (fromLink $ apiLink (Proxy @PageBlogsAPI) Nothing (Just $ T.toLower t))
            $ fromText t

homePage :: Member Render r => Maybe BlogEntry -> Sem r Html
homePage be = do
    b' <- mapM toSummary be
    pure $ do
        div introBlock
        h1 "Newest blog"
        div ! A.class_ "blog-list" $
            fromMaybe
                (div ! A.class_ "empty" $ "Looks like there's nothing here yet")
                b'
