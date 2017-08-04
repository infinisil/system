import XMonad
import Data.Monoid
import XMonad.Layout.Fullscreen
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.Spacing
import System.IO
import XMonad.Prompt
import XMonad.Prompt.Shell
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import GHC.Word
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Tabbed
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
import XMonad.Prompt.Pass
import qualified Text.Fuzzy as Fuzz


-- Makes every window transparent
setTransparentHook :: Event -> X All
setTransparentHook ConfigureEvent{ev_event_type = createNotify, ev_window = id} = do
  setOpacity id opacity
  return (All True) where
    opacityFloat = 0.95
    opacity = floor $ fromIntegral (maxBound :: Word32) * opacityFloat
    setOpacity id op = spawn $ "xprop -id " ++ show id ++ " -f _NET_WM_WINDOW_OPACITY 32c -set _NET_WM_WINDOW_OPACITY " ++ show op
setTransparentHook _ = return (All True)

layout = a ||| b ||| (noBorders $ avoidStruts Full)
  where
     tiled   = Tall nmaster delta ratio
     mod l = avoidStruts $ spacing 5 $ l
     a = mod tiled
     b = mod $ Mirror tiled
     nmaster = 1
     ratio   = 1/2
     delta = 3/100

c1 = "#6F1313"
c2 = "#A93316"
c3 = "#EF9930"
c4 = "#feca6a"
c5 = "#B96746"

ppconfig :: XPConfig
ppconfig = def
	{ font = "xft: Helvetica Neue LT Std:style=55 Roman,Regular"
	, bgColor = c1
	, fgColor = c4
	, bgHLight = c2
	, fgHLight = c4
	, searchPredicate = Fuzz.test
	, alwaysHighlight = False
	, borderColor = c4
	, promptBorderWidth = 1
	, height = 20
	, maxComplRows = Just 3
	, historyFilter = deleteConsecutive
	}

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm,               xK_c     ), spawn $ XMonad.terminal conf)

    -- launch firefox
    , ((modm,               xK_f     ), spawn "firefox")

    , ((modm, xK_p), passPrompt ppconfig)

		-- Eject zpools
		, ((modm,               xK_e     ), spawn "sudo zpool export betty")

    -- close focused window
    , ((modm,               xK_w     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm .|. shiftMask, xK_Tab   ), windows W.focusUp  )

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    --, ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    , ((modm, xK_a), spawn "echo Win $(date) >> /home/infinisil/Notes/OverwatchStats")
    , ((modm, xK_o), spawn "echo Loss $(date) >> /home/infinisil/Notes/OverwatchStats")

    , ((mod1Mask, xK_space), spawn "dmenu_run")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_ampersand
                                                , xK_bracketleft
                                                , xK_braceleft
                                                , xK_braceright
                                                , xK_parenleft
                                                , xK_equal
                                                , xK_asterisk
                                                , xK_parenright
                                                , xK_plus]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ def
       { terminal = "gnome-terminal"
       , modMask = mod4Mask
       , manageHook = (isFullscreen --> doFullFloat) <+> manageDocks <+> manageHook def
       , layoutHook = lessBorders OnlyFloat $ fullscreenFull layout
       , handleEventHook =
         --setTransparentHook <+>
         fullscreenEventHook <+>
         docksEventHook <+>
         handleEventHook def
       , logHook = dynamicLogWithPP xmobarPP
                       { ppOutput = hPutStrLn xmproc
                       , ppTitle = xmobarColor "green" "" . shorten 50
                       }
       , borderWidth = 2
       , normalBorderColor = "#000000"
       , focusedBorderColor = "#DD5500"
       , keys = myKeys
       }
