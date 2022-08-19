import Data.List (delete)
import Data.Maybe (fromMaybe)
import Graphics.X11
import qualified Text.Fuzzy as Fuzz
import XMonad
import XMonad.Actions.Navigation2D
import XMonad.Hooks.DynamicLog
import qualified XMonad.Hooks.EwmhDesktops as EWMH
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks (avoidStruts, docks)
import XMonad.Layout.BinarySpacePartition
import qualified XMonad.Layout.Fullscreen as Full
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances (StdTransformers (FULL))
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Prompt (XPConfig (..), deleteConsecutive)
import XMonad.Prompt.Pass (passPrompt)
import qualified XMonad.StackSet as W
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.WindowProperties (getProp32)

main :: IO ()
main = xmonad (myConfig layoutSpacing)

layoutSpacing =
  Full.fullscreenFocus
    . noBorders
    . avoidStruts
    . mkToggle (single FULL)
    $ emptyBSP

myNavigation2DConfig :: Navigation2DConfig
myNavigation2DConfig =
  def
    { defaultTiledNavigation = hybridNavigation
    }

myConfig :: l Window -> XConfig l
myConfig l =
  EWMH.ewmh $
    docks $
      withNavigation2DConfig myNavigation2DConfig $
        def
          { terminal = "kitty",
            modMask = case "@modifier@" of
              "1" -> mod1Mask
              "3" -> mod3Mask
              "4" -> mod4Mask
              mod -> error $ "Unsupported modifier: " ++ mod,
            manageHook =
              (title =? "Dunst" --> insertPosition Above Older)
                <+> manageHook def
                <+> Full.fullscreenManageHook,
            layoutHook = l,
            workspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"],
            handleEventHook = Full.fullscreenEventHook,
            logHook =
              dynamicLogString
                xmobarPP
                  { ppTitle = xmobarColor "green" "" . shorten 30
                  }
                >>= xmonadPropLog,
            keys = \c -> mkKeymap c (myKeymap c),
            startupHook =
              setDefaultCursor xC_left_ptr
                <+> EWMH.fullscreenStartup
          }

myKeymap c =
  [ ("M-f", spawn "firefox"),
    ("M-<Space>", spawn "rofi -show run -theme gruvbox-dark"),
    ("M-c", spawn "kitty"),
    ("<XF86AudioPlay>", spawn "mpc sendmessage toggle 1"),
    ("<XF86AudioNext>", spawn "mpc sendmessage playlist next"),
    ("<XF86AudioPrev>", spawn "mpc sendmessage playlist prev"),
    ("<XF86AudioLowerVolume>", spawn "xmonad-volume lower"),
    ("<XF86AudioRaiseVolume>", spawn "xmonad-volume raise"),
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
    ("@switch0@", windows $ W.greedyView (XMonad.workspaces c !! 0)),
    ("@switch1@", windows $ W.greedyView (XMonad.workspaces c !! 1)),
    ("@switch2@", windows $ W.greedyView (XMonad.workspaces c !! 2)),
    ("@switch3@", windows $ W.greedyView (XMonad.workspaces c !! 3)),
    ("@switch4@", windows $ W.greedyView (XMonad.workspaces c !! 4)),
    ("@switch5@", windows $ W.greedyView (XMonad.workspaces c !! 5)),
    ("@switch6@", windows $ W.greedyView (XMonad.workspaces c !! 6)),
    ("@switch7@", windows $ W.greedyView (XMonad.workspaces c !! 7)),
    ("@switch8@", windows $ W.greedyView (XMonad.workspaces c !! 8)),
    ("@switch9@", windows $ W.greedyView (XMonad.workspaces c !! 9)),
    ("@switch10@", windows $ W.greedyView (XMonad.workspaces c !! 10)),
    ("@switch11@", windows $ W.greedyView (XMonad.workspaces c !! 11)),
    ("@shift0@", windows $ W.shift (XMonad.workspaces c !! 0)),
    ("@shift1@", windows $ W.shift (XMonad.workspaces c !! 1)),
    ("@shift2@", windows $ W.shift (XMonad.workspaces c !! 2)),
    ("@shift3@", windows $ W.shift (XMonad.workspaces c !! 3)),
    ("@shift4@", windows $ W.shift (XMonad.workspaces c !! 4)),
    ("@shift5@", windows $ W.shift (XMonad.workspaces c !! 5)),
    ("@shift6@", windows $ W.shift (XMonad.workspaces c !! 6)),
    ("@shift7@", windows $ W.shift (XMonad.workspaces c !! 7)),
    ("@shift8@", windows $ W.shift (XMonad.workspaces c !! 8)),
    ("@shift9@", windows $ W.shift (XMonad.workspaces c !! 9)),
    ("@shift10@", windows $ W.shift (XMonad.workspaces c !! 10)),
    ("@shift11@", windows $ W.shift (XMonad.workspaces c !! 11))
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
      promptBorderWidth = 1,
      height = 20,
      maxComplRows = Just 5,
      historyFilter = deleteConsecutive
    }
