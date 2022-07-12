module PersonalWebsite.CSS.Parser where

import qualified Data.Attoparsec.ByteString.Char8 as C8
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as B
import Optics
import Relude

data Attribute = Attribute {key :: !Text, value :: !Text} deriving (Show)

makeFieldLabelsWith noPrefixFieldLabels ''Attribute

renderAttribute :: Attribute -> B.Builder
renderAttribute (Attribute k v) = B.fromText k <> ":" <> B.fromText v <> ";"

comment :: Parser ()
comment = void $ string "/*" *> manyTill anyChar (string "*/")

ws :: Parser ()
ws = skipMany (void space <|> comment)

takeTill1 :: (Char -> Bool) -> Parser Text
takeTill1 p = takeWhile1 (not . p)

attribute :: Parser Attribute
attribute = do
    k <- takeTill1 (\x -> (x == ':') || C8.isSpace x) <?> "attribute key"
    ws *> skip (== ':') <* ws <?> "attribute seperator"
    v <- takeTill1 (== ';') <?> "attribute value"
    skip (== ';') <?> "semi colon"
    pure $ Attribute k v

newtype Selector = Selector (NonEmpty Text) deriving (Show)

renderSelector :: Selector -> B.Builder
renderSelector (Selector l) = mconcat $ intersperse "," (B.fromText <$> toList l)

selector :: Parser Selector
selector = do
    (sel : sels) <- oneSelector `sepBy1` char ','
    pure $ Selector $ sel :| sels
  where
    oneSelector = do
        sel <- takeTill1 (\c -> c == '{' || c == ',') <?> "selector name"
        when ("}" `T.isInfixOf` sel) (fail "not a selector")
        pure sel

data CSSBlock = CSSBlock
    { selectors :: Selector
    , attributes :: [Attribute]
    }
    deriving (Show)

makeFieldLabelsWith noPrefixFieldLabels ''CSSBlock

renderCSSBlock :: CSSBlock -> B.Builder
renderCSSBlock (CSSBlock sel attrs) =
    mconcat
        [ renderSelector sel
        , "{"
        , mconcat $ renderAttribute <$> attrs
        , "}"
        ]

cssBlock :: Parser CSSBlock
cssBlock = do
    _ <- lookAhead (notChar '@')
    sel <- try selector
    _ <- ws *> char '{' <?> "start of block"
    attrs <- many (try (ws *> attribute <* ws)) <?> "attributes"
    _ <- ws *> char '}'
    pure $ CSSBlock sel attrs

data MediaQuery = MediaQuery {query :: Text, rules :: [CSSBlock]} deriving (Show)

makeFieldLabelsWith noPrefixFieldLabels ''MediaQuery

renderMediaQuery :: MediaQuery -> B.Builder
renderMediaQuery (MediaQuery q bs) =
    mconcat
        [ B.fromText q
        , "{"
        , mconcat $ renderCSSBlock <$> bs
        , "}"
        ]

mediaQuery :: Parser MediaQuery
mediaQuery = do
    query' <- char '@' *> takeTill1 (== '{')
    blocks <- char '{' *> ws *> many cssBlock <* ws <* char '}' <?> "blocks"
    pure $ MediaQuery ("@" <> toText query') blocks

type Block = Either MediaQuery CSSBlock

renderBlock :: Either MediaQuery CSSBlock -> B.Builder
renderBlock = either renderMediaQuery renderCSSBlock

block :: Parser Block
block = ws *> eitherP (try (mediaQuery <?> "media query")) (try (cssBlock <?> "rules block")) <* ws

renderCSS :: [Block] -> B.Builder
renderCSS = mconcat . fmap renderBlock

css :: Parser [Block]
css = many (try (block <?> "block"))
