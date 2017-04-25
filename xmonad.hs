import XMonad

main = xmonad defaultConfig
       { terminal = "sakura"
       , modMask = mod4Mask
       }
