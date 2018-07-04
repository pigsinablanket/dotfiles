import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

-- The main function.
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- Command to launch the bar.
myBar = "xmobar"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor "#ffff00" "" . wrap "|" "|" }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Main configuration, override the defaults to your liking.
myConfig = defaultConfig
  { modMask = mod4Mask
  , terminal = "lxterminal"
} `additionalKeys`
  [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock"),
    ((mod4Mask .|. shiftMask, xK_f), spawn "firefox"),
    ((0               , 0x1008FF11), spawn "amixer -q sset Master 2%-"),
    ((0               , 0x1008FF13), spawn "amixer -q sset Master 2%+"),
    ((0               , 0x1008FF12), spawn "amixer set Master toggle"),
    ((0               , 0x1008FFB2), spawn "amixer -q sset Capture toggle"),
    ((0               , 0x1008FF02), spawn "xbacklight -inc 5"),
    ((0               , 0x1008FF03), spawn "xbacklight -dec 5")
--    ((0               , 0xffeb),     spawn "thunar")
  ]
