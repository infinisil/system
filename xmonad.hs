import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import XMonad.Prompt
import XMonad.Prompt.Shell

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ def
       { terminal = "sakura"
       , modMask = mod4Mask
       , manageHook = manageDocks <+> manageHook def
       , layoutHook = avoidStruts  $  layoutHook def
       , handleEventHook = docksEventHook <+> handleEventHook def
       , logHook = dynamicLogWithPP xmobarPP
                       { ppOutput = hPutStrLn xmproc
                       , ppTitle = xmobarColor "green" "" . shorten 50
                       }
       , borderWidth = 3
       , normalBorderColor = "#000000"
       , focusedBorderColor = "#007FFF"
       }
       `additionalKeys`
       [
         -- ((mod4Mask .|. shiftMask, xK_z), spawn "echo hi")
         -- ((controlMask, xK_Print), spawn "scrot -s")
         -- ((0, xK_Print), spawn "scrot")
         -- ((controlMask .|. mod4Mask, xK_z), spawn "echo \"hi\nthere\" | dmenu")
         ((mod1Mask, xK_space), shellPrompt def)
       ]
