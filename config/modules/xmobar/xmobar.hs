{-# LANGUAGE ViewPatterns #-}

import Data.List (intercalate)
import System.Environment (getArgs)
import Xmobar

main :: IO ()
main = do
  [read -> screenId] <- getArgs
  xmobar (xmobarConfig screenId)

xmobarConfig :: Int -> Config
xmobarConfig screenId =
  defaultConfig
    { font = "xft:" ++ intercalate "," fonts,
      bgColor = "#282828",
      fgColor = "#ebdbb2",
      alpha = 230,
      position = OnScreen screenId (TopH 30),
      commands =
        [ Run $
            Cpu
              [ "-t",
                "\57958 <total>%",
                "-L",
                "10",
                "-H",
                "50",
                "-l",
                "#b8bb26",
                "-n",
                "#fabd2f",
                "-h",
                "#fb4934"
              ]
              10,
          Run $ Date "<fc=#d3869b>%F %T</fc>" "date" 10,
          Run $
            NamedXPropertyLog
              ("_XMONAD_LOG_" ++ show (fromEnum screenId))
              "XMonadLog",
          Run $
            DynNetwork
              [ "-t",
                "\62513 <tx> KB/s <fc=#83a598>|</fc> \62515 <rx> KB/s",
                "-L",
                "10000",
                "-H",
                "500000",
                "-l",
                "#b8bb26",
                "-n",
                "#fabd2f",
                "-h",
                "#fb4934"
              ]
              10,
          Run $ Com "xmobar-batt" [] "bt" 50,
          Run $ PipeReader "${XDG_RUNTIME_DIR}/musicInfo" "info",
          Run $ PipeReader "${XDG_RUNTIME_DIR}/obs" "obs",
        ],
      sepChar = "%",
      alignSep = "}{",
      template = "} %XMonadLog%{%obs% <fc=#83a598>|</fc> %dynnetwork% <fc=#83a598>|</fc> %cpu% <fc=#83a598>|</fc> %bt%%date% "
    }
  where
    -- Check with fc-match "<string>" family size
    -- See all fonts with fc-list
    -- Lower has higher precedence
    fonts =
      [ -- Emoji, though DejaVu also has some :/
        "Noto Color Emoji-14",
        -- Japanese Kanji and Kana
        "M+ 2p-16",
        -- Latin letters
        "DejaVu Sans-14",
        -- Nerdfonts symbols
        "Symbols Nerd Font-14"
      ]
