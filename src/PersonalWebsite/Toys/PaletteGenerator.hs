{-# LANGUAGE TemplateHaskell #-}

module PersonalWebsite.Toys.PaletteGenerator (colorGeneratorPage) where

import qualified Clay as C
import Data.FileEmbed
import Optics
import PersonalWebsite.API
import PersonalWebsite.API.CSS (baseStyle, declareVars)
import PersonalWebsite.Colors
import PersonalWebsite.Colors.Conversion
import PersonalWebsite.HTMX
import PersonalWebsite.Internal
import PersonalWebsite.Pandoc
import PersonalWebsite.Toys.API
import Polysemy
import qualified Polysemy.Input as P
import Polysemy.Reader
import Relude hiding (Reader, ask, div, local, span)
import Skylighting
import Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A

mkSampleBlog :: (Member Render r) => Sem r Html
mkSampleBlog = renderMarkdown $ decodeUtf8 $(embedFile "assets/color-generator-samples.md")

getBG :: (Member (Reader ColorSeed) r) => Sem r C.Color
getBG = askColorPalette <&> view #bg

mkSample :: (Members [Reader ColorSeed, Render] r) => String -> Sem r Html
mkSample className = do
    sampleVarDecls <- declareVars <$> askColorPalette
    sampleCodeStyle <- askCodeStyle
    sampleBlog <- mkSampleBlog
    bg' <- getBG
    pure do
        style
            . toMarkup
            . C.render
            $ fromString ("." <> className)
            C.? ".sample"
            C.& do
                C.pointerEvents C.none
                "user-select" C.-: "none"
                sampleVarDecls
                baseStyle
                paddingRem 1 1 1 1
                C.background bg'
        style
            . toMarkup
            $ scopeCSS ("." <> toText className)
            $ toText
            $ styleToCss sampleCodeStyle
        div ! A.class_ (fromString $ "sample " <> className) $ sampleBlog
        pass

getLightness :: C.Color -> Float
getLightness (C.Hsla _ _ l _) = l
getLightness c = getLightness $ C.toHsla c

shiftBG :: C.Color -> C.Color
shiftBG = setTransparency 0.4 . offsetBGBy 0.5

offsetBGBy :: Float -> C.Color -> C.Color
offsetBGBy d c =
    if getLightness c > 0.5 then C.lighten d c else C.darken d c

mkPageCSS :: (Member (Reader ColorSeed) r) => Sem r C.Css
mkPageCSS = do
    bg' <- shiftBG <$> getBG
    pure do
        ".try-title" <> ".palette-title" C.? do
            C.display C.flex
            C.maxWidth (C.px 768)
            C.marginLeft C.auto
            C.marginRight C.auto
            C.justifyContent C.spaceBetween
            C.alignItems C.center
            C.h1 <> C.h3 C.<? do
                C.important $ C.marginLeft (C.px 0)
                C.important $ C.marginRight (C.px 0)
        ".palette-block" C.<? do
            paddingRem 2 2 2 2
            C.background bg'
            C.h3 C.<? ".palette-title" C.& do
                C.marginLeft (C.px 0)
        ".palette" C.? do
            C.marginBottom (C.rem 1.5)
            C.display C.grid
            "grid-template-columns" C.-: "repeat(auto-fit, minmax(224px, 1fr))"
            ".color-block" C.<? do
                C.display C.grid
                C.marginBottom (C.rem 0.75)
                C.gridTemplateColumns [C.fr 1]
                ".info" C.<? do
                    C.whiteSpace C.nowrap
                    C.display C.flex
                    C.justifyContent C.spaceBetween
                    sequence_ $ [C.paddingRight, C.paddingLeft] ?? C.rem 0.75
                    C.paddingTop (C.rem 0.75)
                ".color" C.<? C.height (C.rem 3)

colorGeneratorPage ::
    (Members [Reader ColorSeed, Render, P.Input Int] r) =>
    Maybe SeedParam ->
    Sem r Html
colorGeneratorPage mSeed = do
    currentPalette <- mkPalette False
    pageCSS <- mkPageCSS
    someRandomNumbers <- replicateM 30 $ P.input @Int
    let joinCommas [] = ""
        joinCommas [x, y] = x <> " and " <> y
        joinCommas (x : xs) = x <> ", " <> joinCommas xs

    testPalette <- forM mSeed \case
        Seed s -> local (const s) $ mkPalette True
        IncorrectSeed t ->
            pure $ div ! A.class_ "palette-block" $ do
                p do
                    "Bad Seed"
                    seedLink
                    toMarkup $ " " <> "'" <> t <> "'"
                p do
                    "You gotta pick a number friend. "
                    "You remember numbers right? The things you use to count. "
                    "In case your forgot I'll help you out. "
                    "Let me give you some numbers you can try. "
                    br
                    text $ joinCommas $ show <$> someRandomNumbers
                    " are all numbers. "
                    "Pick any number you want, world's your oyster!"
    pure $ div do
        style . toMarkup $ C.render pageCSS
        h1 "Current palette"
        currentPalette
        div ! A.class_ "try-title" $ do
            h1 ! A.id "test-palette" $ "Try a palette"
            tryForm
        sequence_ testPalette
  where
    seedLink = a ! A.href "https://www.youtube.com/watch?v=Ahr4KFl79WI" ! A.target "blank" $ ":"
    tryForm =
        form
            ! hxSwap "multi:#main:outerHTML,#header-nav:outerHTML"
            ! hxGet (fromLink $ apiLink (Proxy @ColorGeneratorAPI) Nothing)
            $ do
                input ! A.type_ "submit" ! A.value "try with seed"
                input
                    ! A.type_ "text"
                    ! A.name "seed"
                    ! A.value
                        ( case mSeed of
                            Just (Seed s) -> fromString $ toString $ toText s
                            _ -> ""
                        )

mkPalette :: (Members [Reader ColorSeed, Render] r) => Bool -> Sem r Html
mkPalette showSetPaletteButton = do
    seed <- ask @ColorSeed
    plt <- askColorPalette
    cPlt <- askCodeHighlight
    let blockName = "seeded(" <> toText seed <> ")"
        className = toString $ "seeded" <> toText seed
        paletteCSS =
            fromString ("." <> className) C.? do
                ".bg" C.? ".color" C.? C.background (plt ^. #bg)
                ".fg1" C.? ".color" C.? C.background (plt ^. #fg1)
                ".fg2" C.? ".color" C.? C.background (plt ^. #fg2)
                ".primary" C.? ".color" C.? C.background (plt ^. #primary)
                ".highlight" C.? ".color" C.? C.background (plt ^. #highlight)
    sample <- mkSample className
    pure
        $ div
        ! A.class_ "palette-block"
        $ do
            style . toMarkup $ C.render paletteCSS
            div ! A.class_ "palette-title" $ do
                h3 $ toMarkup blockName
                when showSetPaletteButton (setSeedForm seed)
            h4 "regular palette"
            div ! A.class_ (fromString $ "palette " <> className) $ do
                mapM_
                    colorBlock
                    [ ("background", plt ^. #bg)
                    , ("regular text", plt ^. #fg1)
                    , ("links", plt ^. #fg2)
                    , ("accent", plt ^. #primary)
                    , ("highlight", plt ^. #highlight)
                    ]
            h4 "code highlighting"
            div ! A.class_ (fromString $ "palette " <> className) $ do
                mapM_
                    colorBlock
                    [ ("background", viewCl cPlt #background)
                    , ("default", viewCl cPlt #defaultC)
                    , ("key/const/operator", viewCl cPlt #keyConstOp)
                    , ("declaration", viewCl cPlt #dataDec)
                    , ("number", viewCl cPlt #baseFloat)
                    , ("string/char", viewCl cPlt #charString)
                    , ("comment", viewCl cPlt #comment)
                    , ("alert/warning", viewCl cPlt #alertWarn)
                    , ("error", viewCl cPlt #errorC)
                    , ("function", viewCl cPlt #function)
                    ]
            sample
  where
    viewCl plt l = maybe (C.rgb 0 0 0) skylightingToClay (plt ^. l)
    setSeedForm s = form
        ! hxPost "/set-seed"
        ! hxSwap "multi:#var-declarations:outerHtml,#code-style:outerHtml,#favicon-link:outerHtml"
        $ do
            input
                ! A.class_ "no-display"
                ! A.type_ "text"
                ! A.name "seed"
                ! A.value (fromString $ toString $ toText s)
            input ! A.type_ "submit" ! A.value "set this palette"
    bgStyle cl = fromString $ toString $ "background: " <> C.plain (coerce $ C.value cl)
    colorBlock (t, cl) = div ! A.class_ "color-block" $ do
        div ! A.class_ "color" ! A.style (bgStyle cl) $ pass
        div ! A.class_ "info" $ do
            div t
            div $ toMarkup $ clayToText cl
