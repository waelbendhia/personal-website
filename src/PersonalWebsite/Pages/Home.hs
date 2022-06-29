module PersonalWebsite.Pages.Home (homePage) where

import PersonalWebsite.Pages.Blog (BlogEntry, toSummary)
import Relude hiding (div, span)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal
import Text.Blaze.XHtml5
import Text.Pandoc

introBlock :: Html
introBlock = div ! A.class_ "intro-block" $ do
    p "Hello. I'm Wael. This is my personal website. "
    p $ do
        "I'll be posting things here about various topics. "
        "I write mostly about programming, mainly about "
        a ! A.href "/blog?tag=haskell" $ "Haskell"
        " or "
        a ! A.href "/blog?tag=go" $ "Go"
        " and maybe some "
        a ! A.href "/blog?tag=javascript" $ "Javascript"
        "/"
        a ! A.href "/blog?tag=typescript" $ "Typescript"
        "."

homePage :: PandocMonad m => Maybe BlogEntry -> m Html
homePage be = do
    b' <- mapM toSummary be
    pure $ do
        div introBlock
        h1 "Newest blog"
        div ! A.class_ "blog-list" $
            fromMaybe
                (div ! A.class_ "empty" $ "Looks like there's nothing here yet")
                b'
