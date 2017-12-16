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
import           XMonad.Layout.WindowNavigation
import           XMonad.Prompt
import           XMonad.Prompt.Pass
import           XMonad.Prompt.XMonad
import qualified XMonad.StackSet                     as W
import           XMonad.Util.Cursor
import           XMonad.Util.EZConfig

layout =
  let
    tiled = Tall 1 (3/100) (1/2)
    mods = avoidStruts . smartBorders . smartSpacing 1
  in
    mods $ mkToggle (single FULL) (Mirror tiled ||| tiled) ||| Full ||| MosaicAlt M.empty

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
  [ ("M4-S-l", windows W.swapDown  )
  , ("M4-S-h", windows W.swapUp    )
  , ("M4-,", sendMessage (IncMasterN 1))
  , ("M4-.", sendMessage (IncMasterN (-1)))
  , ("M4-f", spawn "@firefox@")
  , ("M4-w", kill)
  --, ("<Break> w", kill)
  , ("M4-<Space>", sendMessage NextLayout)
  , ("M4-<Tab>", windows W.focusDown)
  , ("M4-S-<Tab>", windows W.focusUp)
  , ("M4-r", spawn "@dmenu_run@")
  , ("M4-c", spawn "@konsole@")
  , ("M4-<Return>", windows W.swapMaster)
  , ("M4-p", passPrompt ppconfig)
  , ("M4-e", spawn "@emacs@")
  , ("M4-t", withFocused $ windows . W.sink)
  , ("M4-j", sendMessage Shrink)
  , ("M4-k", sendMessage Expand)
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
  , ("<Break> t l", spawn "if [[ $(systemctl --user is-active live-wallpaper) = active ]]; then systemctl --user stop live-wallpaper; else systemctl --user start live-wallpaper; fi")
  , ("<Break> t p", spawn "@toggle@")
  , ("<Break> t c", spawn "@toggleCompton@")
  , ("<XF86AudioPlay>", spawn "@playpause@")
  , ("<XF86AudioNext>", spawn "@mpc_cli@/bin/mpc next")
  , ("<XF86AudioPrev>", spawn "@mpc_cli@/bin/mpc prev")
  , ("<XF86AudioRaiseVolume>", spawn "@volup@")
  , ("<XF86AudioLowerVolume>", spawn "@voldown@")
  , ("<XF86AudioMute>", spawn "@mutetoggle@")
  , ("<XF86MonBrightnessUp>", spawn "@brightup@")
  , ("<XF86MonBrightnessDown>", spawn "@brightdown@")
  ] ++
  [ ("<Break> w h", withFocused (sendMessage . shrinkWindowAlt))
  , ("<Break> w l", withFocused (sendMessage . expandWindowAlt))
  , ("<Break> w k", withFocused (sendMessage . tallWindowAlt))
  , ("<Break> w j", withFocused (sendMessage . wideWindowAlt))
  , ("<Break> w <Space>", sendMessage resetAlt)
  ] ++ [
  ("M4-" ++ shift ++ key, windows $ f i) |
    (i, key) <- zip (XMonad.workspaces c) (map (:[]) "&[{}(=*)+") , (f, shift) <- [(W.greedyView, ""), (W.shift, "S-")]]


main :: IO ()
main = xmonad $ EWMH.ewmh $ docks $ myConfig

myConfig = def
    { terminal = "@konsole@"
    , modMask = mod4Mask
    , manageHook =
      (title =? "Dunst" --> insertPosition Above Older) <+>
      (isFullscreen --> doFullFloat) <+>
      manageHook def
    , layoutHook = layout
    , handleEventHook =
      handleEventHook def <+>
      EWMH.fullscreenEventHook
    , logHook = dynamicLogString xmobarPP
      { ppTitle = xmobarColor "green" "" . shorten 30
      } >>= xmonadPropLog
    , borderWidth = 2
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#DD5500"
    , keys = \c -> mkKeymap c (myKeymap c)
    , startupHook = do
        return ()
        checkKeymap myConfig (myKeymap myConfig)
        setDefaultCursor xC_left_ptr
    }
