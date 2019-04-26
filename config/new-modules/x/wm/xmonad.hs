{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
import qualified Data.Map                            as M
import           Data.Maybe
import           Graphics.X11.Types
import           System.Environment                  (getEnv)
import qualified Text.Fuzzy                          as Fuzz
import           Text.Read                           (readMaybe)
import           XMonad
import           XMonad.Actions.DeManage
import           XMonad.Actions.Navigation2D
import           XMonad.Hooks.DynamicLog
import qualified XMonad.Hooks.EwmhDesktops           as EWMH
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
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

import           XMonad.Layout.IndependentScreens    (countScreens,
                                                      onCurrentScreen,
                                                      withScreens, workspaces')

import           Control.Monad                       (when)
import           Data.Semigroup
import           XMonad.Actions.CopyWindow

layoutSpacing =
  mods $ mkToggle (single FULL) emptyBSP
    where
      mods = avoidStruts . smartBorders . smartSpacing 2

layout =
  mods $ mkToggle (single FULL) emptyBSP
    where
      mods = avoidStruts . smartBorders

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

myKeymap c n =
  [ ("M4-f", spawn "@firefox@")
  , ("M4-w", kill)
  , ("M4-<Space>", spawn "@run@")
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
  , ("<Break> s b", spawn "@baka@")
  , ("M4-n", spawn "@nope@")
  , ("<Break> m t", spawn "@tag@")
  , ("<Break> b h", spawn "home-manager switch")
  , ("<Break> r x", spawn "xmonad --restart && systemctl --user restart xmobar")
  , ("<Break> t b", spawn "@toggleXmobar@")
  , ("<Break> t l", spawn "@toggleLive@")
  , ("<Break> t p", spawn "@toggle@")
  , ("<Break> t c", spawn "@toggleCompton@")
  , ("<F8>", spawn "@playpause@")
  , ("<F9>", spawn "@next@")
  , ("<F7>", spawn "@prev@")
  , ("<F12>", spawn "@volUp@")
  , ("<F11>", spawn "@volDown@")
  , ("<F10>", spawn "@toggleMute@")
  , ("<F2>", spawn "@brightUp@")
  , ("<F1>", spawn "@brightDown@")
  ] ++
  [ ("M4-<Tab>", windows W.focusDown)
  , ("M4-S-<Tab>", windows W.focusUp)
  , ("M4-a", sendMessage SelectNode)
  , ("M4-o", sendMessage MoveNode)
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
  , ("M4-d", withFocused demanage)
  ] ++ [
  ("M4-" ++ shift ++ key, windows $ (if n == 1 then id else onCurrentScreen) f i) |
    (i, key) <- zip (if n == 1 then XMonad.workspaces c else workspaces' c) (map (:[]) "&[{}(=*)+") , (f, shift) <- [(W.greedyView, ""), (W.shift, "S-")]]

myNavigation2DConfig :: Navigation2DConfig
myNavigation2DConfig = def
  { defaultTiledNavigation = hybridNavigation
  }

main :: IO ()
main = do
  nScreens <- countScreens
  let spacing = fromMaybe False . readMaybe $ "@spacing@"
  if spacing
    then xmonad $ EWMH.ewmh $ docks $ withNavigation2DConfig myNavigation2DConfig $ myConfig nScreens layoutSpacing
    else xmonad $ EWMH.ewmh $ docks $ withNavigation2DConfig myNavigation2DConfig $ myConfig nScreens layout

copyWindowToAll :: (Eq a, Eq i, Eq s) => a -> W.StackSet i l a s sd -> W.StackSet i l a s sd
copyWindowToAll w s = foldr (copyWindow w . W.tag) s (W.workspaces s)

copyShivacam :: Event -> X All
copyShivacam MapRequestEvent { ev_window = w } = runQuery title w >>= \case
  "copyall" -> withDisplay $ \dpy -> do
    withWindowAttributes dpy w $ \wa -> do
      managed <- isClient w
      when (not (wa_override_redirect wa) && not managed) $ manage w

    windows $ copyWindowToAll w
    return $ All False
  _ -> return $ All True
copyShivacam _ = return $ All True


myConfig :: ScreenId -> l Window -> XConfig l
myConfig n l = def
    { terminal = "@terminal@"
    , modMask = mod4Mask
    , manageHook =
      (title =? "copyall" --> doFloat) <+>
      (title =? "Dunst" --> insertPosition Above Older) <+>
      (isFullscreen --> doFullFloat) <+>
      manageHook def
    , layoutHook = l
    , workspaces = (if n == 1 then id else withScreens n) ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
    , handleEventHook =
      copyShivacam <+>
      EWMH.fullscreenEventHook <+>
      EWMH.ewmhDesktopsEventHook
    , logHook = dynamicLogString xmobarPP
      { ppTitle = xmobarColor "green" "" . shorten 30
      } >>= xmonadPropLog
    , borderWidth = 2
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#FFFFFF"
    , keys = \c -> mkKeymap c (myKeymap c n)
    , startupHook = do
        return ()
        --checkKeymap myConfig (myKeymap myConfig)
        setDefaultCursor xC_left_ptr
    }
