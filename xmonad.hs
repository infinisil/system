import XMonad
import XMonad.Config.Xfce

main = xmonad xfceConfig
       { terminal = "sakura"
       , modMask = mod4Mask
       }
