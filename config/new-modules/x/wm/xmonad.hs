{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
import qualified Data.Map                            as M
import           Data.Maybe
import           Graphics.X11.Types
import           System.Environment                  (getEnv)
import qualified Text.Fuzzy                          as Fuzz
import           Text.Read                           (readMaybe)
import           XMonad
import           XMonad.Util.WindowProperties (getProp32)
import           XMonad.Actions.DeManage
import           XMonad.Prelude (delete)
import           XMonad.Actions.Navigation2D
import           XMonad.Hooks.DynamicLog
import qualified XMonad.Hooks.EwmhDesktops           as EWMH
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.BoringWindows
import           XMonad.Layout.CenteredMaster
import           XMonad.Layout.Fullscreen as Full
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
  Full.fullscreenFocus .
  noBorders .
  avoidStruts .
  mkToggle (single FULL)
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

myKeymap c n =
  [ ("M4-f", spawn "@firefox@")
  , ("M4-w", kill)
  , ("M4-<Space>", spawn "@run@")
  , ("M4-c", spawn "kitty")
  , ("M4-i", spawn "@irc@")
  , ("M4-p", passPrompt ppconfig)
  , ("M4-t", withFocused $ windows . W.sink)
  , ("M4-m", withDisplay $ \dpy -> withFocused $ \win -> do
      wmstate <- getAtom "_NET_WM_STATE"
      fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"
      wstate <- fromMaybe [] <$> getProp32 wmstate win
      let isFull = fromIntegral fullsc `elem` wstate
          chWState f = io $ changeProperty32 dpy win wmstate aTOM propModeReplace (f wstate)
      if isFull then do
        chWState $ delete (fromIntegral fullsc)
        broadcastMessage $ Full.RemoveFullscreen win
        sendMessage Full.FullscreenChanged
      else
        sendMessage $ Toggle FULL
    )
  , ("<XF86AudioPlay>", spawn "mpc sendmessage toggle 1")
  , ("<XF86AudioNext>", spawn "mpc sendmessage playlist next")
  , ("<XF86AudioPrev>", spawn "mpc sendmessage playlist prev")
  , ("<XF86AudioRaiseVolume>", spawn "@volUp@")
  , ("<XF86AudioLowerVolume>", spawn "@volDown@")
  , ("<XF86AudioMute>", spawn "@toggleMute@")
  , ("M4-<Tab>", windows W.focusDown)
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
  , ("M4-S-7", windows $ W.greedyView (XMonad.workspaces c !! 0))
  , ("M4-[", windows $ W.greedyView (XMonad.workspaces c !! 1))
  , ("M4-S-[", windows $ W.greedyView (XMonad.workspaces c !! 2))
  , ("M4-S-]", windows $ W.greedyView (XMonad.workspaces c !! 3))
  , ("M4-S-9", windows $ W.greedyView (XMonad.workspaces c !! 4))
  , ("M4-=", windows $ W.greedyView (XMonad.workspaces c !! 5))
  , ("M4-S-8", windows $ W.greedyView (XMonad.workspaces c !! 6))
  , ("M4-S-0", windows $ W.greedyView (XMonad.workspaces c !! 7))
  , ("M4-S-=", windows $ W.greedyView (XMonad.workspaces c !! 8))
  , ("M4-S-5", windows $ W.shift (XMonad.workspaces c !! 0))
  , ("M4-7", windows $ W.shift (XMonad.workspaces c !! 1))
  , ("M4-5", windows $ W.shift (XMonad.workspaces c !! 2))
  , ("M4-3", windows $ W.shift (XMonad.workspaces c !! 3))
  , ("M4-1", windows $ W.shift (XMonad.workspaces c !! 4))
  , ("M4-9", windows $ W.shift (XMonad.workspaces c !! 5))
  , ("M4-0", windows $ W.shift (XMonad.workspaces c !! 6))
  , ("M4-2", windows $ W.shift (XMonad.workspaces c !! 7))
  , ("M4-4", windows $ W.shift (XMonad.workspaces c !! 8))
  ]

myNavigation2DConfig :: Navigation2DConfig
myNavigation2DConfig = def
  { defaultTiledNavigation = hybridNavigation
  }

main :: IO ()
main = do
  nScreens <- countScreens
  xmonad $ EWMH.ewmh $ docks $ withNavigation2DConfig myNavigation2DConfig $ myConfig nScreens layoutSpacing

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
    , modMask = mod4Mask
    , manageHook =
      (title =? "copyall" --> doFloat) <+>
      (title =? "Dunst" --> insertPosition Above Older) <+>
      manageHook def <+>
      Full.fullscreenManageHook
    , layoutHook = l
    , workspaces = (if n == 1 then id else withScreens n) ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
    , handleEventHook = -- copyShivacam
        Full.fullscreenEventHook
    , logHook = dynamicLogString xmobarPP
      { ppTitle = xmobarColor "green" "" . shorten 30
      } >>= xmonadPropLog
    , borderWidth = 2
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#FFFFFF"
    , keys = \c -> mkKeymap c (myKeymap c n)
    , startupHook =
      setDefaultCursor xC_left_ptr <+>
        EWMH.fullscreenStartup
    }
