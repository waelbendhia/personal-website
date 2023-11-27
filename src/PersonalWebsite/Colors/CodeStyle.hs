module PersonalWebsite.Colors.CodeStyle (
    askCodeStyle,
    askCodeHighlight,
    CodeHighlight (..),
    styleToClay,
) where

import qualified Clay as C
import Optics
import PersonalWebsite.Colors.Conversion
import PersonalWebsite.Colors.Data
import PersonalWebsite.Colors.RandomGen
import qualified PersonalWebsite.Random as R
import Polysemy
import Polysemy.Reader
import Relude hiding (Reader, asks)
import Relude.Extra
import Skylighting
import System.Random

data CodeHighlight = CodeHighlight
    { background :: Maybe Color
    , defaultC :: Maybe Color
    , keyConstOp :: Maybe Color
    , dataDec :: Maybe Color
    , baseFloat :: Maybe Color
    , charString :: Maybe Color
    , comment :: Maybe Color
    , alertWarn :: Maybe Color
    , errorC :: Maybe Color
    , function :: Maybe Color
    }

makeFieldLabelsWith noPrefixFieldLabels ''CodeHighlight

styleFromHighlight :: CodeHighlight -> Style
styleFromHighlight ch =
    Style
        { backgroundColor = background ch
        , defaultColor = defaultC ch
        , lineNumberColor = Nothing
        , lineNumberBackgroundColor = Nothing
        , tokenStyles =
            fromList
                [ (KeywordTok, defStyle{tokenColor = keyConstOp ch})
                , (DataTypeTok, defStyle{tokenColor = dataDec ch})
                , (DecValTok, defStyle{tokenColor = dataDec ch})
                , (BaseNTok, defStyle{tokenColor = baseFloat ch})
                , (FloatTok, defStyle{tokenColor = baseFloat ch})
                , (CharTok, defStyle{tokenColor = charString ch})
                , (StringTok, defStyle{tokenColor = charString ch})
                , (SpecialStringTok, defStyle{tokenColor = charString ch})
                , (VerbatimStringTok, defStyle{tokenColor = charString ch})
                , (CommentTok, defStyle{tokenColor = comment ch})
                , (AlertTok, defStyle{tokenColor = alertWarn ch})
                , (FunctionTok, defStyle{tokenColor = function ch})
                , (ErrorTok, defStyle{tokenColor = errorC ch})
                , (WarningTok, defStyle{tokenColor = alertWarn ch, tokenBold = True})
                , (ConstantTok, defStyle{tokenColor = keyConstOp ch, tokenBold = True})
                , (ImportTok, defStyle)
                , (VariableTok, defStyle)
                , (OperatorTok, defStyle{tokenColor = keyConstOp ch})
                , (ControlFlowTok, defStyle{tokenColor = keyConstOp ch})
                , (BuiltInTok, defStyle)
                , (ExtensionTok, defStyle)
                , (AttributeTok, defStyle)
                ]
        }

randomHightlight ::
    (RandomGen g) => (Integer, Float, Float) -> R.RandomT g CodeHighlight
randomHightlight (bgh, bgs, bgl) =
    CodeHighlight
        <$> randomSK (bgh, bgs, codeBGL) (30, 10, 10)
        <*> randomSK (bgh + 120, bgs / 2, codeFGL) (10, 10, 5)
        <*> randomSK (bgh + 180, bgs / 2, codeFGL) (10, 10, 5)
        <*> randomSK (bgh - 120, bgs / 2, codeFGL) (10, 10, 5)
        <*> randomSK (bgh - 60, bgs / 2, codeFGL) (10, 10, 5)
        <*> randomSK (bgh + 60, bgs / 2, codeFGL) (10, 10, 5)
        <*> randomSK (bgh + 120, bgs / 4, (codeFGL + codeBGL) / 2) (10, 10, 5)
        <*> randomSK (40, bgs, codeFGL) (10, 10, 5)
        <*> randomSK (0, bgs, codeFGL) (10, 10, 5)
        <*> randomSK (bgh + 120, bgs * 0.75, codeFGL) (10, 10, 5)
  where
    randomSK a b = hslToSkylighting <$> randomHSLAColorJitter a b
    codeBGL = if bgl > 50 then (bgl + 100) / 2 else bgl / 2
    codeFGL = if bgl > 50 then bgl / 2 else (bgl + 100) / 2

askCodeHighlight :: (Member (Reader ColorSeed) r) => Sem r CodeHighlight
askCodeHighlight = asks @ColorSeed $ \s ->
    let (bg', s') =
            R.withRandom (mkStdGen $ coerce s)
                $ R.randomRM ((0, 0, 0), (255, 100, 100))
     in fst $ R.withRandom s' $ randomHightlight bg'

askCodeStyle :: (Member (Reader ColorSeed) r) => Sem r Style
askCodeStyle = styleFromHighlight <$> askCodeHighlight

toCss :: (TokenType, TokenStyle) -> C.Css
toCss (t, tf) =
    C.code C.** C.span C.# short t C.? do
        mapM_ (C.color . skylightingToClay) $ tokenColor tf
        mapM_ (C.backgroundColor . skylightingToClay) $ tokenBackground tf
        when (tokenBold tf) $ C.fontWeight C.bold
        when (tokenItalic tf) $ C.fontStyle C.italic
        when (tokenUnderline tf) (C.textDecoration C.underline)

styleToClay :: Style -> C.Css
styleToClay f = do
    divspec
    numberspec
    colorspec
    mapM_ toCss (toPairs $ tokenStyles f)
  where
    colorspec =
        C.div C.# ".sourceCode" C.? do
            mapM_ (C.color . skylightingToClay) $ defaultColor f
            mapM_ (C.backgroundColor . skylightingToClay) $ backgroundColor f
    divspec = do
        C.pre C.|> C.code C.# ".sourceCode" C.? do
            C.whiteSpace C.preWrap
            C.position C.relative
            C.span C.<? do
                C.display C.inlineBlock
                C.lineHeight (C.rem 1.25)
                C.empty C.& C.height (C.rem 1.25)
        ".sourceCode" C.? C.overflow C.visible
        C.code C.# ".sourceCode" C.|> C.span C.? do
            C.color C.inherit
            C.textDecoration C.inherit
        C.div C.# ".sourceCode" C.? do
            C.margin (C.rem 1) (C.rem 0) (C.rem 1) (C.rem 0)
            C.overflow C.auto
        C.pre C.# ".sourceCode" C.? C.margin (C.rem 0) (C.rem 0) (C.rem 1) (C.rem 0)
    numberspec = do
        C.pre C.# ".numberSource" C.** C.code C.? do
            "counter-reset" C.-: "source-line 0"
            C.span C.<? do
                C.position C.relative
                C.left (C.rem (-4))
                "counter-increment" C.-: "source-line"
                C.a C.# C.firstChild C.# C.before C.<? do
                    "content" C.-: "counter(source-line)"
                    C.position C.relative
                    C.left (C.rem (-1))
                    C.textAlign C.end
                    C.verticalAlign C.vAlignBaseline
                    C.border (C.px 0) C.none C.black
                    C.display C.inlineBlock
                    "-webkit-touch-callout" C.-: "none"
                    "-webkit-user-select" C.-: "none"
                    "-khtml-user-select" C.-: "none"
                    "-moz-user-select" C.-: "none"
                    "-ms-user-select" C.-: "none"
                    "user-select" C.-: "none"
                    C.padding (C.px 0) (C.rem 0.25) (C.px 0) (C.rem 0.25)
                    mapM_ (C.backgroundColor . skylightingToClay) (lineNumberBackgroundColor f)
                    mapM_ (C.color . skylightingToClay) (lineNumberColor f)
            C.pre C.# ".numberSource" C.? do
                C.marginLeft (C.rem 3)
                mapM_ (C.borderLeft (C.px 1) C.solid . skylightingToClay) (lineNumberColor f)
                C.paddingLeft (C.rem 0.25)

short :: TokenType -> C.Refinement
short KeywordTok = ".kw"
short DataTypeTok = ".dt"
short DecValTok = ".dv"
short BaseNTok = ".bn"
short FloatTok = ".fl"
short CharTok = ".ch"
short StringTok = ".st"
short CommentTok = ".co"
short OtherTok = ".ot"
short AlertTok = ".al"
short FunctionTok = ".fu"
short RegionMarkerTok = ".re"
short ErrorTok = ".er"
short ConstantTok = ".cn"
short SpecialCharTok = ".sc"
short VerbatimStringTok = ".vs"
short SpecialStringTok = ".ss"
short ImportTok = ".im"
short DocumentationTok = ".do"
short AnnotationTok = ".an"
short CommentVarTok = ".cv"
short VariableTok = ".va"
short ControlFlowTok = ".cf"
short OperatorTok = ".op"
short BuiltInTok = ".bu"
short ExtensionTok = ".ex"
short PreprocessorTok = ".pp"
short AttributeTok = ".at"
short InformationTok = ".in"
short WarningTok = ".wa"
short NormalTok = ""
