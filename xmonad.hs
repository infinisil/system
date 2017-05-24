import XMonad
import Data.Monoid
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

myEventHook :: Event -> X All
myEventHook ConfigureEvent{ ev_event_type = createNotify, ev_window = id } = do
  spawn $ "compton-trans -w " ++ show id ++ " 50"
  return (All True)
myEventHook _ = return (All True)

-- Makes every window transparent
setTransparentHook :: Event -> X All
setTransparentHook ConfigureEvent{ev_event_type = createNotify, ev_window = id} = do
  setOpacity id opacity
  return (All True) where
    opacityFloat = 0.85
    opacity = floor $ fromIntegral (maxBound :: Word32) * opacityFloat
    setOpacity id op = spawn $ "xprop -id " ++ show id ++ " -f _NET_WM_WINDOW_OPACITY 32c -set _NET_WM_WINDOW_OPACITY " ++ show op
setTransparentHook _ = return (All True)

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ def
       { terminal = "sakura"
       , modMask = mod4Mask
       , manageHook = manageDocks <+> manageHook def
       , layoutHook = avoidStruts $ spacing 5 $ layoutHook def
       , handleEventHook = setTransparentHook <+> docksEventHook <+> handleEventHook def
       , logHook = dynamicLogWithPP xmobarPP
                       { ppOutput = hPutStrLn xmproc
                       , ppTitle = xmobarColor "green" "" . shorten 50
                       }
       , borderWidth = 3
       , normalBorderColor = "#000000"
       , focusedBorderColor = "#DD5500"
       }
       `additionalKeys`
       [
         -- ((mod4Mask .|. shiftMask, xK_z), spawn "echo hi")
         -- ((controlMask, xK_Print), spawn "scrot -s")
         -- ((0, xK_Print), spawn "scrot")
         -- ((controlMask .|. mod4Mask, xK_z), spawn "echo \"hi\nthere\" | dmenu")
         ((mod1Mask, xK_space), shellPrompt def)
       ]
