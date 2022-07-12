module PersonalWebsite.Colors.CodeStyle (
    askCodeStyle,
    askCodeHighlight,
    CodeHighlight (..),
) where

import Capability.Reader
import Optics
import PersonalWebsite.Colors.Conversion
import PersonalWebsite.Colors.RandomGen
import qualified PersonalWebsite.Random as R
import Relude hiding (asks)
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

randomHightlight :: RandomGen g => (Integer, Float, Float) -> R.RandomT g CodeHighlight
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
    randomSK a b = clayToSkylighting <$> randomHSLAColorJitter a b
    codeBGL = if bgl > 50 then (bgl + 100) / 2 else bgl / 2
    codeFGL = if bgl > 50 then bgl / 2 else (bgl + 100) / 2

askCodeHighlight :: HasReader "colorSeed" Int m => m CodeHighlight
askCodeHighlight = asks @"colorSeed" $ \s ->
    let (bg', s') = R.withRandom (mkStdGen s) $ R.randomRM ((0, 0, 0), (255, 100, 100))
     in fst $ R.withRandom s' $ randomHightlight bg'

askCodeStyle :: HasReader "colorSeed" Int m => m Style
askCodeStyle = styleFromHighlight <$> askCodeHighlight
