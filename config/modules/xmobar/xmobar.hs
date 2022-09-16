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
      bgColor = "#2b2b29",
      fgColor = "#c3ae93",
      alpha = 210,
      position = OnScreen screenId Top,
      commands =
        [ Run $
            Cpu
              [ "-t",
                "<total>%",
                "-L",
                "10",
                "-H",
                "50",
                "-l",
                "green",
                "-h",
                "red"
              ]
              10,
          -- , Run CoreTemp
          --   [ "-t", "<core0>/<core1>°C "
          --   , "-L", "65"
          --   ,	"-H", "90"
          --   ,	"-l", "lightblue"
          --   ,	"-h", "red" ] 50
          Run $ Date "%a %d.%m %T" "date" 10,
          Run $
            NamedXPropertyLog
              ("_XMONAD_LOG_" ++ show (fromEnum screenId))
              "XMonadLog",
          Run $
            Memory [] 10,
          Run $
            DynNetwork
              [ "-t",
                "Up: <tx> KB/s, Down: <rx> KB/s | ",
                "-L",
                "10000",
                "-H",
                "500000",
                "-l",
                "green",
                "-n",
                "orange",
                "-h",
                "red"
              ]
              10,
          Run $ Com "xmobar-power" [] "power" 10,
          Run $ Com "xmobar-batt" [] "bt" 50,
          Run $ Com "wpctl" ["get-volume", "@DEFAULT_AUDIO_SINK@"] "volume" 10,
          Run $ PipeReader "${XDG_RUNTIME_DIR}/musicInfo" "info"
        ],
      sepChar = "%",
      alignSep = "}{",
      template = "%XMonadLog% %info%}{ %power%A \57354 | %volume% | %memory% | %dynnetwork%%cpu% \62171 | %bt% | <fc=#ee9a00>%date%</fc>"
    }
  where
    -- Check with fc-match "<string>" family size
    -- Or fc-list
    fonts =
      [ -- Latin letters
        "DejaVu Sans-14",
        -- Nerdfonts symbols
        "FantasqueSansMono Nerd Font-14",
        -- Japanese Kanji and Kana
        "M+ 2p-16",
        -- Emoji, though DejaVu also has some :/
        "Noto Color Emoji-14"
      ]
