module PersonalWebsite.API.CSS (baseStyle, declareVars) where

import Clay as Cl
import Optics hiding (pre, (#), (&), (|>))
import PersonalWebsite.Colors hiding (background)
import PersonalWebsite.Internal
import Relude hiding (Reader, ask, div, rem, (&), (**))

serifFont :: Css
serifFont = fontFamily ["Iosevka Etoile"] [serif]

sansSerifFont :: Css
sansSerifFont = fontFamily ["Iosevka Aile"] [sansSerif]

monospaceFont :: Css
monospaceFont = fontFamily ["Iosevka"] [monospace]

allLangs :: [Text]
allLangs =
    [ "json"
    , "yaml"
    , "javascript"
    , "typescript"
    , "jsx"
    , "tsx"
    , "go"
    , "lua"
    , "haskell"
    , "idris"
    , "elm"
    , "purescript"
    ]

pltVarNames :: Palette_ String
pltVarNames =
    asName
        <$> Palette
            { bg = "bg"
            , bgTransparent = "bg-transparent"
            , fg1 = "fg1"
            , fg2 = "fg2"
            , primary = "primary"
            , highlight = "highlight"
            }
  where
    asName varName = "--" <> varName <> "-color"

pltVars :: Palette_ Color
pltVars = asColor <$> pltVarNames
  where
    asColor varName = Other $ fromString $ "var(" <> varName <> ")"

declareVars :: Palette -> Css
declareVars plt =
    root html do
        declVar (view #bg)
        declVar (view #bgTransparent)
        declVar (view #fg1)
        declVar (view #fg2)
        declVar (view #primary)
        declVar (view #highlight)
  where
    declVar :: (forall a. Palette_ a -> a) -> Css
    declVar sel = fromString (sel pltVarNames) -: plain (unValue $ value $ sel plt)

codeStyle :: Css
codeStyle = do
    code ? monospaceFont
    div # ".sourceCode" ? do
        border (px 1) solid (pltVars ^. #fg1)
        marginRem 1.5 0 2 0
        marginBottom (rem 2)
        position relative
        forM_ allLangs $ \l ->
            star # byClass l ? ("font-feature-settings" -: fontFeature l)
        ".langtag" <? do
            position absolute
            right (px 0)
            top (px 0)
            paddingRem 0.25 0.25 0.25 0.5
        pre <? do
            marginRem 2 0.75 0.75 0.75
            mapM_ langBefore allLangs
            before & do
                position absolute
                left (px 0)
                top (px 0)
                paddingRem 0.25 0.5 0.25 0.25
  where
    langBefore l = fromString (toString $ "." <> l) <> before & content (stringContent l)
    fontFeature x
        | x `elem` ["javascript", "typescript", "jsx", "tsx", "go", "lua"] =
            "\"calt\" 0, \"CLIK\" 1"
        | x `elem` ["haskell", "idris", "elm", "purescript"] =
            "\"calt\" 0, \"HSKL\" 1"
        | otherwise = "\"calt\" 1"

flexRow :: Css
flexRow = do
    display flex
    flexDirection row
    alignItems center

flexCol :: Css
flexCol = do
    display flex
    flexDirection column

blogStyle :: Css
blogStyle = do
    a <> button <> (".tag-header" ** ".tag") ? monospaceFont
    (".tag-header" ** ".tag") ? color (pltVars ^. #highlight)
    ".blog-list" ? do
        ".seperator" ? do
            background (pltVars ^. #fg2)
            height (rem 0.25)
            sequence_ $ [marginLeft, marginRight, marginTop] <&> ($ rem 1)
        margin (px 0) auto (px 0) auto
        maxWidth (px 768)
        ".blog-item" <? do
            ".summary" <? do
                overflow hidden
                position relative
                maxHeight (px 352)
                marginBottom (rem 1.5)
                before & do
                    content (stringContent "")
                    display block
                    height (rem 4)
                    position absolute
                    mapM_ ($ px 0) [bottom, left]
                    background
                        $ linearGradient
                            (angular $ deg 0)
                            [(pltVars ^. #bg, 0), (transparent, 100)]
                    width (pct 100)
                    zIndex 2
            ".link-row" <? do
                flexRow
                justifyContent flexEnd
                marginTop (rem 0.75)
                fontSize (rem 1.125)
                a <> button <? do
                    display block
                    paddingRem 0.25 0.25 0.25 0.25
                    fontWeight bold
    ".metadata" ? do
        flexRow
        justifyContent spaceBetween
        ".tags" <? do
            flexRow
            "gap" -: "0.5rem"

tagsStyle :: Css
tagsStyle =
    ".tags" ? do
        flexRow
        fontSize (rem 1.25)
        "gap" -: "1rem"
        "flex-wrap" -: "wrap"

baseStyle :: Css
baseStyle = do
    star ? transition "color, background" (ms 150) ease (ms 0)
    html ? do
        "scroll-padding" -: "3rem"
        overflow auto
        height (pct 100)
    button ? do
        background (none @BackgroundImage)
        color inherit
        border (px 0) none transparent
        paddingRem 0 0 0 0
        cursor pointer
        outline none (px 0) transparent
        fontSize (rem 1)
    color (pltVars ^. #fg1)
    codeStyle
    blogStyle
    tagsStyle
    footer ? do
        flexShrink 1
        paddingRem 0 1 1.5 1
        marginTop (rem 3)
        div <? do
            flexRow
            justifyContent spaceBetween
            marginLeft auto
            marginRight auto
            marginTop (rem 1)
            maxWidth (px 1024)
    input ? ".no-display" & display none
    ".toc" ? do
        paddingRem 0.5 1 1 1
        li <? do
            forM_ [1 :: Int .. 6] \lvl ->
                byClass ("level-" <> show lvl) & paddingLeft (rem $ fromIntegral lvl)
            marginBottom (rem 0.4)
            listStyleType none
            fontSize (rem 1.2)
    "input[type=text]" ? paddingLeft (rem 0.75)
    "input[type=submit]" ? ":hover" & do
        color (pltVars ^. #primary)
        borderBottomColor (pltVars ^. #primary)
        cursor pointer
    input <? do
        serifFont
        boxSizing borderBox
        "border" -: "none"
        "background" -: "none"
        paddingTop (rem 0)
        paddingBottom (rem 0)
        height (rem 3)
        color (pltVars ^. #fg2)
        fontSize (rem 1.125)
        fontWeight (weight 400)
        borderBottom (rem 0.25) solid (pltVars ^. #fg2)
    ".lost" <> ".empty" <? do
        paddingTop (rem 4)
        paddingBottom (rem 4)
        textAlign center
        a <> button ? fontWeight bold
    a <> button ? linkStyle
    h1 <> h2 <> h3 <> h4 <> header ? serifFont
    header ? do
        "backdrop-filter" -: "blur(4px)"
        flexRow
        justifyContent spaceBetween
        flexGrow 0
        width (pct 100)
        maxWidth (px 1024)
        whiteSpace nowrap
        zIndex 4
        background $ pltVars ^. #bgTransparent
        height (rem 3)
        lineHeight (rem 3)
        paddingRem 0 2 0 2
        position sticky
        top (px 0)
        margin (px 0) auto (px 0) auto
        ".title" <? do
            flexRow
            height (rem 3)
            "gap" -: "1 rem"
            form <? do
                marginRem 0 0 0 0
                height (rem 3)
                input <? width (rem 3)
        h1 <? do
            fontSize (rem 1.5)
            fontWeight (weight 500)
            marginRem 0 0 0 0
        div # ".nav" |> (a <> button) <? do
            textDecoration none
            fontSize (rem 1.125)
            fontWeight (weight 400)
            paddingRem 0 0.5 0 0.5
            height (rem 2.75)
            display inlineBlock
            ".active" & do
                borderBottom (rem 0.25) solid (pltVars ^. #fg2)
                ":hover" & borderBottomColor (pltVars ^. #primary)
        h2 <? do
            flexShrink 1
            overflow hidden
            textOverflow overflowEllipsis
    body ? do
        flexCol
        background (pltVars ^. #bg)
        color (pltVars ^. #fg1)
        sansSerifFont
        marginRem 0 0 0 0
        minHeight (pct 100)
    main_ <? do
        flexGrow 1
        paddingRem 1.5 1 1.5 1
        maxWidth (px 1024)
        width (pct 100)
        margin (px 0) auto (px 0) auto
        p ? marginBottom (rem 2)
        h1 <> h2 <> h3 <> h4 <> p <> li <> ".metadata" ? do
            maxWidth (px 768)
            marginLeft auto
            marginRight auto
    ".intro-block" ? do
        marginTop (rem 1.5)
        fontSize (rem 1.125)
    ".pager" ? do
        flexRow
        justifyContent spaceBetween
        height (rem 6)
        fontSize (rem 1.125)
        (a <> button) # ".disabled" <? do
            textDecoration lineThrough
            pointerEvents none
  where
    linkStyle = do
        textDecoration underline
        color (pltVars ^. #fg2)
        ":hover" & color (pltVars ^. #primary)
