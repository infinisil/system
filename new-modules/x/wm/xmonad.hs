{-# LANGUAGE FlexibleContexts #-}
import qualified Data.Map                            as M
import           Graphics.X11.Types
import qualified Text.Fuzzy                          as Fuzz
import           XMonad
import           XMonad.Hooks.DynamicLog
import qualified XMonad.Hooks.EwmhDesktops           as EWMH
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Actions.Navigation2D
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.BoringWindows
import           XMonad.Layout.CenteredMaster
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.MosaicAlt
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.Named
import           XMonad.Layout.NoBorders             (noBorders, smartBorders)
import           XMonad.Layout.Spacing               (smartSpacing)
import           XMonad.Layout.SubLayouts
import           XMonad.Prompt
import           XMonad.Prompt.Pass
import           XMonad.Prompt.XMonad
import qualified XMonad.StackSet                     as W
import           XMonad.Util.Cursor
import           XMonad.Util.EZConfig

import XMonad.Layout.IndependentScreens (withScreens, onCurrentScreen, workspaces', countScreens)

layout =
  let
    -- tiled = Tall 1 (3/100) (1/2)
    mods = avoidStruts . smartBorders
  in
    mods $ mkToggle (single FULL) emptyBSP
    -- mods $ mkToggle (single FULL) (Mirror tiled ||| tiled) ||| Full ||| MosaicAlt M.empty

c1 = "#6F1313"
c2 = "#A93316"
c3 = "#EF9930"
c4 = "#feca6a"
c5 = "#B96746"

ppconfig :: XPConfig
ppconfig = def
  { font = "xft: FuraMono Nerd Font:style=Medium,Regular:pixelsize=14"
  , bgColor = "#2b2b29"
  , fgColor = "#c3ae93"
  , bgHLight = "#575743"
  , fgHLight = "#e3ab66"
  , searchPredicate = Fuzz.test
  , alwaysHighlight = False
  , borderColor = c4
  , promptBorderWidth = 1
  , height = 20
  , maxComplRows = Just 5
  , historyFilter = deleteConsecutive
  }

myKeymap c =
  [ ("M4-f", spawn "@firefox@")
  , ("M4-w", kill)
  , ("M4-<Space>", spawn "@dmenu_run@")
  , ("M4-c", spawn "@terminal@")
  , ("M4-i", spawn "@irc@")
  , ("M4-p", passPrompt ppconfig)
  , ("M4-e", spawn "@emacs@")
  , ("M4-t", withFocused $ windows . W.sink)
  , ("M4-m", sendMessage $ Toggle FULL)
  , ("<Break> x", xmonadPrompt def)
  , ("<Break> s m", spawn "@mobile@")
  , ("<Break> s d", spawn "@desktop@")
  , ("<Break> m ;", spawn "@rate1@")
  , ("<Break> m ,", spawn "@rate2@")
  , ("<Break> m .", spawn "@rate3@")
  , ("<Break> m p", spawn "@rate4@")
  , ("<Break> m y", spawn "@rate5@")
  , ("<Break> m f", spawn "@rate6@")
  , ("<Break> m g", spawn "@rate7@")
  , ("<Break> m c", spawn "@rate8@")
  , ("<Break> m r", spawn "@rate9@")
  , ("<Break> m l", spawn "@rate10@")
  , ("<Break> m n", spawn "@nope@")
  , ("<Break> s n", spawn "@nani@")
  , ("<Break> s e", spawn "@explosion@")
  , ("M4-n", spawn "@nope@")
  , ("<Break> m t", spawn "@tag@")
  , ("<Break> b h", spawn "home-manager switch")
  , ("<Break> r x", spawn "xmonad --restart && systemctl --user restart xmobar")
  , ("<Break> t b", spawn "@toggleXmobar@")
  , ("<Break> t l", spawn "@toggleLive@")
  , ("<Break> t p", spawn "@toggle@")
  , ("<Break> t c", spawn "@toggleCompton@")
  , ("<XF86AudioPlay>", spawn "@playpause@")
  , ("<XF86AudioNext>", spawn "@next@")
  , ("<XF86AudioPrev>", spawn "@prev@")
  , ("<XF86AudioRaiseVolume>", spawn "@volUp@")
  , ("<XF86AudioLowerVolume>", spawn "@volDown@")
  , ("<XF86AudioMute>", spawn "@toggleMute@")
  , ("<XF86MonBrightnessUp>", spawn "@brightUp@")
  , ("<XF86MonBrightnessDown>", spawn "@brightDown@")
  ] ++
  [ ("M4-<Tab>", windows W.focusDown)
  , ("M4-S-<Tab>", windows W.focusUp)
  , ("M4-S-l", sendMessage $ MoveSplit R)
  , ("M4-S-h", sendMessage $ MoveSplit L)
  , ("M4-S-j", sendMessage $ MoveSplit D)
  , ("M4-S-k", sendMessage $ MoveSplit U)
  , ("M4-u", sendMessage FocusParent)
  , ("M4-l", windowGo R False)
  , ("M4-h", windowGo L False)
  , ("M4-j", windowGo D False)
  , ("M4-k", windowGo U False)
  , ("M4-C-l", windowSwap R False)
  , ("M4-C-h", windowSwap L False)
  , ("M4-C-j", windowSwap D False)
  , ("M4-C-k", windowSwap U False)
  , ("M4-s", sendMessage Swap)
  , ("M4-r", sendMessage Rotate)
  , ("M4-b b", sendMessage Balance)
  , ("M4-b e", sendMessage Equalize)
  ] ++ [
  ("M4-" ++ shift ++ key, windows $ onCurrentScreen f i) |
    (i, key) <- zip (workspaces' c) (map (:[]) "&[{}(=*)+") , (f, shift) <- [(W.greedyView, ""), (W.shift, "S-")]]

myNavigation2DConfig :: Navigation2DConfig
myNavigation2DConfig = def
  { defaultTiledNavigation = hybridNavigation
  }

main :: IO ()
main = do
  nScreens <- countScreens
  xmonad $ EWMH.ewmh $ docks $ withNavigation2DConfig myNavigation2DConfig $ myConfig nScreens

myConfig n = def
    { terminal = "@terminal@"
    , modMask = mod4Mask
    , manageHook =
      (title =? "Dunst" --> insertPosition Above Older) <+>
      (isFullscreen --> doFullFloat) <+>
      manageHook def
    , layoutHook = layout
    , workspaces = withScreens n ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
    , handleEventHook =
      handleEventHook def <+>
      EWMH.fullscreenEventHook
    , logHook = dynamicLogString xmobarPP
      { ppTitle = xmobarColor "green" "" . shorten 30
      } >>= xmonadPropLog
    , borderWidth = 2
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#FFFFFF"
    , keys = \c -> mkKeymap c (myKeymap c)
    , startupHook = do
        return ()
        --checkKeymap myConfig (myKeymap myConfig)
        setDefaultCursor xC_left_ptr
    }
