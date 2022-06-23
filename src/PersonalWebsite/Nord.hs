module PersonalWebsite.Nord (nord) where

import Skylighting.Core

import Relude

color :: Int -> Maybe Color
color = toColor

nord :: Style
nord =
    Style
        { backgroundColor = color 0x2e3440
        , defaultColor = color 0xeceff4
        , lineNumberColor = Nothing
        , lineNumberBackgroundColor = Nothing
        , tokenStyles =
            fromList
                [ (KeywordTok, defStyle{tokenColor = color 0x81A1C1})
                , (DataTypeTok, defStyle{tokenColor = color 0x8FBCBB})
                , (DecValTok, defStyle{tokenColor = color 0x8fbcbb})
                , (BaseNTok, defStyle{tokenColor = color 0xb48ead})
                , (FloatTok, defStyle{tokenColor = color 0xb48ead})
                , (CharTok, defStyle{tokenColor = color 0xa3be8c})
                , (StringTok, defStyle{tokenColor = color 0xa3be8c})
                , (CommentTok, defStyle{tokenColor = color 0x616E88})
                , (AlertTok, defStyle{tokenColor = color 0xebcb8b})
                , (FunctionTok, defStyle{tokenColor = color 0x88C0D0})
                , (ErrorTok, defStyle{tokenColor = color 0xbf616a})
                , (WarningTok, defStyle{tokenColor = color 0xebcb8b, tokenBold = True})
                , (ConstantTok, defStyle{tokenColor = color 0x81a1c1, tokenBold = True})
                , (ImportTok, defStyle)
                , (VariableTok, defStyle)
                , (OperatorTok, defStyle{tokenColor = color 0x81A1C1})
                , (BuiltInTok, defStyle)
                , (ExtensionTok, defStyle)
                , (AttributeTok, defStyle)
                ]
        }
