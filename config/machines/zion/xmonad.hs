{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
import qualified Data.Map                            as M
import           Data.Maybe
import           Graphics.X11.Types
import           System.Environment                  (getEnv)
import qualified Text.Fuzzy                          as Fuzz
import           Text.Read                           (readMaybe)
import           XMonad
import           Graphics.X11.ExtraTypes.XF86
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
  avoidStruts
  . smartBorders
  . smartSpacing 2
  . mkToggle (single FULL)
  $ emptyBSP

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

myNavigation2DConfig :: Navigation2DConfig
myNavigation2DConfig = def
  { defaultTiledNavigation = hybridNavigation
  }

main :: IO ()
main = do
  nScreens <- countScreens
  dirs <- getDirectories
  launch
    (EWMH.ewmhFullscreen $ EWMH.ewmh $ docks $ withNavigation2DConfig myNavigation2DConfig $ myConfig nScreens layoutSpacing)
    dirs

-- copyWindowToAll :: (Eq a, Eq i, Eq s) => a -> W.StackSet i l a s sd -> W.StackSet i l a s sd
-- copyWindowToAll w s = foldr (copyWindow w . W.tag) s (W.workspaces s)

-- copyShivacam :: Event -> X All
-- copyShivacam MapRequestEvent { ev_window = w } = runQuery title w >>= \case
--   "copyall" -> withDisplay $ \dpy -> do
--     withWindowAttributes dpy w $ \wa -> do
--       managed <- isClient w
--       when (not (wa_override_redirect wa) && not managed) $ manage w
--
--     windows $ copyWindowToAll w
--     return $ All False
--   _ -> return $ All True
-- copyShivacam _ = return $ All True


myConfig :: ScreenId -> l Window -> XConfig l
myConfig n l = def
    { terminal = "kitty"
    --, modMask = mod3Mask
    , manageHook =
      (title =? "copyall" --> doFloat) <+>
      (title =? "Dunst" --> insertPosition Above Older) <+>
      -- (isFullscreen --> doFullFloat) <+>
      manageHook def
    , layoutHook = l
    , workspaces = (if n == 1 then id else withScreens n) ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
    -- , handleEventHook = copyShivacam
    , logHook = dynamicLogString xmobarPP
      { ppTitle = xmobarColor "green" "" . shorten 30
      } >>= xmonadPropLog
    , borderWidth = 2
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#FFFFFF"
    , keys = \c@(XConfig {modMask = modMask}) -> M.fromList $
      [ ((modMask, xK_f), spawn "@firefox@")
      , ((modMask, xK_w), kill)
      , ((modMask, xK_space), spawn "@run@")
      , ((modMask, xK_c), spawn "kitty")
      , ((modMask, xK_i), spawn "@irc@")
      , ((modMask, xK_p), passPrompt ppconfig)
      , ((modMask, xK_t), withFocused $ windows . W.sink)
      , ((modMask, xK_m), sendMessage $ Toggle FULL)
      , ((0, xF86XK_AudioPlay), spawn "mpc sendmessage toggle 1")
      , ((0, xF86XK_AudioNext), spawn "mpc sendmessage playlist next")
      , ((0, xF86XK_AudioPrev), spawn "mpc sendmessage playlist prev")
      , ((0, xF86XK_AudioRaiseVolume), spawn "@volUp@")
      , ((0, xF86XK_AudioLowerVolume), spawn "@volDown@")
      , ((0, xF86XK_AudioMute), spawn "@toggleMute@")
      , ((modMask, xK_Tab), windows W.focusDown)
      , ((modMask .|. shiftMask, xK_Tab), windows W.focusUp)
      , ((modMask, xK_a), sendMessage SelectNode)
      , ((modMask, xK_o), sendMessage MoveNode)
      , ((modMask .|. shiftMask, xK_l), sendMessage $ MoveSplit R)
      , ((modMask .|. shiftMask, xK_h), sendMessage $ MoveSplit L)
      , ((modMask .|. shiftMask, xK_j), sendMessage $ MoveSplit D)
      , ((modMask .|. shiftMask, xK_k), sendMessage $ MoveSplit U)
      , ((modMask, xK_u), sendMessage FocusParent)
      , ((modMask, xK_l), windowGo R False)
      , ((modMask, xK_h), windowGo L False)
      , ((modMask, xK_j), windowGo D False)
      , ((modMask, xK_k), windowGo U False)
      , ((modMask .|. controlMask, xK_l), windowSwap R False)
      , ((modMask .|. controlMask, xK_h), windowSwap L False)
      , ((modMask .|. controlMask, xK_j), windowSwap D False)
      , ((modMask .|. controlMask, xK_k), windowSwap U False)
      , ((modMask, xK_s), sendMessage Swap)
      , ((modMask, xK_r), sendMessage Rotate)
      --, ("M-b b", sendMessage Balance)
      --, ("M-b e", sendMessage Equalize)
      , ((modMask .|. shiftMask, xK_1), windows $ W.greedyView (XMonad.workspaces c !! 0))
      , ((modMask .|. shiftMask, xK_2), windows $ W.greedyView (XMonad.workspaces c !! 1))
      , ((modMask .|. shiftMask, xK_3), windows $ W.greedyView (XMonad.workspaces c !! 2))
      , ((modMask .|. shiftMask, xK_4), windows $ W.greedyView (XMonad.workspaces c !! 3))
      , ((modMask .|. shiftMask, xK_5), windows $ W.greedyView (XMonad.workspaces c !! 4))
      , ((modMask .|. shiftMask, xK_6), windows $ W.greedyView (XMonad.workspaces c !! 5))
      , ((modMask .|. shiftMask, xK_7), windows $ W.greedyView (XMonad.workspaces c !! 6))
      , ((modMask .|. shiftMask, xK_8), windows $ W.greedyView (XMonad.workspaces c !! 7))
      , ((modMask .|. shiftMask, xK_9), windows $ W.greedyView (XMonad.workspaces c !! 8))
      , ((modMask, xK_ampersand), windows $ W.shift (XMonad.workspaces c !! 0))
      , ((modMask, xK_bracketleft), windows $ W.shift (XMonad.workspaces c !! 1))
      , ((modMask, xK_braceleft), windows $ W.shift (XMonad.workspaces c !! 2))
      , ((modMask, xK_braceright), windows $ W.shift (XMonad.workspaces c !! 3))
      , ((modMask, xK_parenleft), windows $ W.shift (XMonad.workspaces c !! 4))
      , ((modMask, xK_equal), windows $ W.shift (XMonad.workspaces c !! 5))
      , ((modMask, xK_asterisk), windows $ W.shift (XMonad.workspaces c !! 6))
      , ((modMask, xK_parenright), windows $ W.shift (XMonad.workspaces c !! 7))
      , ((modMask, xK_plus), windows $ W.shift (XMonad.workspaces c !! 8))
      ]
    , startupHook = setDefaultCursor xC_left_ptr
    }
