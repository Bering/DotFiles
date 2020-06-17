import System.Exit
import XMonad
import XMonad.Util.EZConfig
import XMonad.Hooks.EwmhDesktops
import qualified XMonad.StackSet as W
import XMonad.Util.Paste
import XMonad.Hooks.ManageDocks

myTerminal = "alacritty"

myWorkspaces = map show [1..12]

myKeysToRemove = [ "M-S-<Return>"  -- terminal
                 , "M-S-p"         -- gmrun
                 , "M-S-c"         -- kill
                 , "M-S-q"         -- quit
                 , "M-j"           -- focusDown
                 , "M-k"           -- focusUp
                 , "M-S-j"         -- swapDown
                 , "M-S-k"         -- swapUp
                 , "M-h"           -- Shrink
                 , "M-l"           -- Expand
                 -- workspaces
                 , "M-1", "M-S-1"
                 , "M-2", "M-S-2"
                 , "M-3", "M-S-3"
                 , "M-4", "M-S-4"
                 , "M-5", "M-S-5"
                 , "M-6", "M-S-6"
                 , "M-7", "M-S-7"
                 , "M-8", "M-S-8"
                 , "M-9", "M-S-9"
                 ]

myAdditionalKeys = [ ("M-S-c", io exitSuccess)
                   -- navigation
                   , ("M-<Up>", windows W.focusUp)
                   , ("M-<Down>", windows W.focusDown)
                   , ("M-S-<Up>", windows W.swapUp)
                   , ("M-S-<Down>", windows W.swapDown)
                   , ("M-<Left>", sendMessage Shrink)
                   , ("M-<Right>", sendMessage Expand)
                   -- shortcuts
                   , ("M-1", spawn myTerminal)
                   , ("M-2", spawn "nautilus")
                   , ("M-3", spawn "firefox")
                   , ("M-4", spawn "steam")
                   -- misc
                   , ("M-c", kill)
                   , ("M-v", pasteSelection)
                   ]
                   -- workspaces (f1 - f12)
                   ++
                   [ ("M-" ++ otherModMasks ++ key, action tag)
                     | (tag, key)  <- zip myWorkspaces (map (\x -> "<F" ++ show x ++ ">") [1..12])
                     , (otherModMasks, action) <- [ ("", windows . W.greedyView) -- or W.view
                                                  , ("S-", windows . W.shift)]
                   ]

myStartupHook = do
                  spawn "feh --no-fehbg --bg-scale '/home/phil/Images/camo tech MSI.jpg'"
                  spawn "xsetroot -cursor_name left_ptr"
                  spawn "killall picom; picom &"

main = do
        xmonad $ ewmh def
          { terminal = myTerminal
          , modMask = mod4Mask
          , borderWidth = 2
          , workspaces = myWorkspaces
          , startupHook = myStartupHook
          }
          `removeKeysP` myKeysToRemove
          `additionalKeysP` myAdditionalKeys
