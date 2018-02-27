import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog

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
  }
