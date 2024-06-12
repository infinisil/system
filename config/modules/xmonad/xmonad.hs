{-# LANGUAGE FlexibleContexts #-}

import Control.Concurrent (forkIO)
import Control.Monad (forM_)
import Data.List (delete, intercalate)
import Data.Maybe (fromMaybe)
import Graphics.X11
import System.Exit (exitSuccess)
import Text.Fuzzy qualified as Fuzz
import XMonad
import XMonad.Actions.Commands (defaultCommands, runCommand, runCommandConfig)
import XMonad.Actions.Navigation2D
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops qualified as EWMH
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks (avoidStruts, docks)
import XMonad.Hooks.StatusBar (StatusBarConfig, statusBarProp, statusBarPropTo, withSB)
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Fullscreen qualified as Full
import XMonad.Layout.IndependentScreens
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances (StdTransformers (FULL))
import XMonad.Layout.NoBorders (smartBorders, withBorder)
import XMonad.Prompt (XPConfig (..), deleteConsecutive)
import XMonad.Prompt.Pass (passPrompt)
import XMonad.StackSet qualified as W
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.Dmenu (menuArgs)
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.WindowProperties (getProp32)

main :: IO ()
main = do
  screenCount <- countScreens
  xmonad (myConfig screenCount layoutSpacing)

layoutSpacing =
  Full.fullscreenFocus
    . smartBorders
    . withBorder 2
    . avoidStruts
    . mkToggle (single FULL)
    $ emptyBSP

myNavigation2DConfig :: Navigation2DConfig
myNavigation2DConfig =
  def
    { defaultTiledNavigation = hybridOf lineNavigation centerNavigation
    }

pp :: PP
pp =
  def
    { ppTitle = xmobarColor "#d3869b" "" . shorten 40,
      ppCurrent = xmobarColor "#fabd2f" "" . wrap "[" "]",
      ppVisible = wrap "[" "]",
      ppSep = " <fc=#83a598>|</fc> "
    }

sb :: ScreenId -> StatusBarConfig
sb screenIndex = statusBarPropTo ("_XMONAD_LOG_" ++ indexStr) command $ pure (marshallPP screenIndex pp)
  where
    command = "xmobar-custom " ++ indexStr
    indexStr = show (fromEnum screenIndex)

myConfig :: LayoutClass l Window => ScreenId -> l Window -> XConfig l
myConfig screenCount l =
  EWMH.ewmh $
    docks $
      withSB (foldMap sb [0 .. (screenCount - 1)]) $
        withNavigation2DConfig myNavigation2DConfig $
          def
            { terminal = "kitty",
              modMask = mod1Mask,
              manageHook =
                (title =? "Dunst" --> insertPosition Above Older)
                  <+> manageHook def
                  <+> Full.fullscreenManageHook,
              layoutHook = l,
              workspaces = withScreens screenCount ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"],
              handleEventHook = Full.fullscreenEventHook,
              keys = \c -> mkKeymap c (myKeymap c),
              startupHook =
                setDefaultCursor xC_left_ptr
                  <+> EWMH.fullscreenStartup
                  <+> spawn "systemctl --user restart random-background"
            }

commands :: X [(String, X ())]
commands =
  return
    [ ("logout", io exitSuccess),
      ("lock", spawn "light-locker-command -l"),
      ("suspend", spawn "systemctl suspend"),
      ("hibernate", spawn "systemctl hibernate"),
      ("suspend-then-hibernate", spawn "systemctl suspend-then-hibernate"),
      ("poweroff", spawn "systemctl poweroff"),
      ("reboot", spawn "systemctl reboot -i")
    ]

rofiCommand :: [String] -> X String
rofiCommand = menuArgs "rofi" ["-dmenu"]

myKeymap :: XConfig l -> [(String, X ())]
myKeymap c =
  [ ("M-S-q", commands >>= runCommandConfig rofiCommand),
    ("M-f", spawn "firefox"),
    ("M-<Space>", spawn "rofi -show run -theme gruvbox-dark"),
    ("M-c", spawn "kitty"),
    ("<XF86AudioPlay>", spawn "mpc sendmessage toggle 1"),
    ("<XF86AudioNext>", spawn "mpc sendmessage playlist next"),
    ("<XF86AudioPrev>", spawn "mpc sendmessage playlist prev"),
    ("<XF86AudioLowerVolume>", spawn "xmonad-volume lower"),
    ("<XF86AudioRaiseVolume>", spawn "xmonad-volume raise"),
    ("S-<XF86AudioLowerVolume>", spawn "mpc sendmessage playlist silentnext"),
    ("S-<XF86AudioRaiseVolume>", spawn "mpc sendmessage playlist next"),
    ("<XF86AudioMute>", spawn "xmonad-mute"),
    ("<XF86MonBrightnessDown>", spawn "busctl --user call org.clight.clight /org/clight/clight org.clight.clight DecBl d 0.1"),
    ("<XF86MonBrightnessUp>", spawn "busctl --user call org.clight.clight /org/clight/clight org.clight.clight IncBl d 0.1"),
    ("M-w", kill),
    ("M-p", passPrompt ppconfig),
    ("M-t", withFocused $ windows . W.sink),
    ( "M-m",
      withDisplay $ \dpy -> withFocused $ \win -> do
        wmstate <- getAtom "_NET_WM_STATE"
        fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"
        wstate <- fromMaybe [] <$> getProp32 wmstate win
        let isFull = fromIntegral fullsc `elem` wstate
            chWState f = io $ changeProperty32 dpy win wmstate aTOM propModeReplace (f wstate)
        if isFull
          then do
            chWState $ delete (fromIntegral fullsc)
            broadcastMessage $ Full.RemoveFullscreen win
            sendMessage Full.FullscreenChanged
          else sendMessage $ Toggle FULL
    ),
    ("M-<Tab>", windows W.focusDown),
    ("M-S-<Tab>", windows W.focusUp),
    ("M-a", sendMessage SelectNode),
    ("M-o", sendMessage MoveNode),
    ("M-S-l", sendMessage $ MoveSplit R),
    ("M-S-h", sendMessage $ MoveSplit L),
    ("M-S-j", sendMessage $ MoveSplit D),
    ("M-S-k", sendMessage $ MoveSplit U),
    ("M-u", sendMessage FocusParent),
    ("M-l", windowGo R False),
    ("M-h", windowGo L False),
    ("M-j", windowGo D False),
    ("M-k", windowGo U False),
    ("M-C-l", windowSwap R False),
    ("M-C-h", windowSwap L False),
    ("M-C-j", windowSwap D False),
    ("M-C-k", windowSwap U False),
    ("M-s", sendMessage Swap),
    ("M-r", sendMessage Rotate),
    ("M-b b", sendMessage Balance),
    ("M-b e", sendMessage Equalize),
    ("M-&", windows $ onCurrentScreen W.greedyView (workspaces' c !! 0)),
    ("M-[", windows $ onCurrentScreen W.greedyView (workspaces' c !! 1)),
    ("M-{", windows $ onCurrentScreen W.greedyView (workspaces' c !! 2)),
    ("M-}", windows $ onCurrentScreen W.greedyView (workspaces' c !! 3)),
    ("M-(", windows $ onCurrentScreen W.greedyView (workspaces' c !! 4)),
    ("M-=", windows $ onCurrentScreen W.greedyView (workspaces' c !! 5)),
    ("M-*", windows $ onCurrentScreen W.greedyView (workspaces' c !! 6)),
    ("M-)", windows $ onCurrentScreen W.greedyView (workspaces' c !! 7)),
    ("M-+", windows $ onCurrentScreen W.greedyView (workspaces' c !! 8)),
    ("M-]", windows $ onCurrentScreen W.greedyView (workspaces' c !! 9)),
    ("M-!", windows $ onCurrentScreen W.greedyView (workspaces' c !! 10)),
    ("M-#", windows $ onCurrentScreen W.greedyView (workspaces' c !! 11)),
    ("M-S-&", windows $ onCurrentScreen W.shift (workspaces' c !! 0)),
    ("M-S-[", windows $ onCurrentScreen W.shift (workspaces' c !! 1)),
    ("M-S-{", windows $ onCurrentScreen W.shift (workspaces' c !! 2)),
    ("M-S-}", windows $ onCurrentScreen W.shift (workspaces' c !! 3)),
    ("M-S-(", windows $ onCurrentScreen W.shift (workspaces' c !! 4)),
    ("M-S-=", windows $ onCurrentScreen W.shift (workspaces' c !! 5)),
    ("M-S-*", windows $ onCurrentScreen W.shift (workspaces' c !! 6)),
    ("M-S-)", windows $ onCurrentScreen W.shift (workspaces' c !! 7)),
    ("M-S-+", windows $ onCurrentScreen W.shift (workspaces' c !! 8)),
    ("M-S-]", windows $ onCurrentScreen W.shift (workspaces' c !! 9)),
    ("M-S-!", windows $ onCurrentScreen W.shift (workspaces' c !! 10)),
    ("M-S-#", windows $ onCurrentScreen W.shift (workspaces' c !! 11))
  ]

ppconfig :: XPConfig
ppconfig =
  def
    { font = "xft: FuraMono Nerd Font:style=Medium,Regular:pixelsize=14",
      bgColor = "#2b2b29",
      fgColor = "#c3ae93",
      bgHLight = "#575743",
      fgHLight = "#e3ab66",
      searchPredicate = Fuzz.test,
      alwaysHighlight = False,
      borderColor = "#feca6a",
      promptBorderWidth = 2,
      height = 25,
      maxComplRows = Just 5,
      historyFilter = deleteConsecutive
    }
