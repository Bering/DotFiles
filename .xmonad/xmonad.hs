import System.Exit
import System.IO (hPutStrLn)
import XMonad
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Util.EZConfig
import XMonad.Util.Paste
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce
import qualified XMonad.StackSet as W

-- TODO:
-- * Struts thigny to reserve space for xmobar
-- * xmobar trayer-padding doesn't work, it shows "Updating..." text
-- * xmobar on all monitors
-- * trayer on all workspaces
-- * trayer not a window (don't switch to it when navigating, never give it focus)
-- * bluetooth systray icon
-- * XMonad.Util.Spotify
-- * Volume keys: , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
-- * pulseaudio tray icon: pa-applet, pasystray or volctl?
-- * Calculator key launch gnome-calculator, and make it float
-- * Lock key lock the session
-- * Home key launch Nautilus
-- * polybar
-- * prompts instead of dmenu?

myTerminal = "alacritty"

myWorkspaces = map show [1..12]

myStartupHook = do
                  spawnOnce "feh --no-fehbg --bg-scale '/home/phil/Images/camo tech MSI.jpg'"
                  spawnOnce "xsetroot -cursor_name left_ptr"
                  spawnOnce "picom &"
                  spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint 0x292d3e --height 18 &"
                  spawnOnce "nm-applet --no-agent &"
                  spawnOnce "pamac-tray"

myLayout = ResizableTall 1 (3/100) (1/2) []
       ||| reflectHoriz (ResizableTall 1 (3/100) (1/2) [])
       ||| Mirror (ResizableTall 1 (3/100) (1/2) [])
       ||| noBorders (Full)


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
                 , "M-,"           -- increase number of clients in master
                 , "M-,"           -- decrease number of clients in master
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
                   , ("M-<Left>", windows W.swapUp)
                   , ("M-<Right>", windows W.swapDown)
                   -- layout manipulation
                   , ("M-S-<Left>", sendMessage Shrink)
                   , ("M-S-<Right>", sendMessage Expand)
                   , ("M-S-<Down>", sendMessage MirrorShrink)
                   , ("M-S-<Up>", sendMessage MirrorExpand)
                   , ("M-<KP_Add>", sendMessage (IncMasterN 1))
                   , ("M-<KP_Subtract>", sendMessage (IncMasterN (-1)))
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

main = do
        xmproc <- spawnPipe "xmobar"
        xmonad $ ewmh def
          { terminal = myTerminal
          , modMask = mod4Mask
          , borderWidth = 2
          , workspaces = myWorkspaces
          , startupHook = myStartupHook
          , layoutHook = myLayout
          , logHook = dynamicLogWithPP xmobarPP
                                          { ppOutput  = \x -> hPutStrLn xmproc x
                                          , ppCurrent = xmobarColor "cyan" "" . wrap "[" "]" -- Current workspace in xmobar
                                          , ppVisible = xmobarColor "cyan" ""                -- Visible but not current workspace
                                          , ppHidden  = xmobarColor "orange" "" . wrap "" ""  -- Hidden workspaces in xmobar
                                          , ppHiddenNoWindows = xmobarColor "gray" ""        -- Hidden workspaces (no windows)
                                          , ppUrgent = xmobarColor "red" "" . wrap "!" "!"  -- Urgent workspace
                                          , ppTitle = xmobarColor "#d0d0d0" "" . shorten 60     -- Title of active window in xmobar
                                          , ppSep =  "<fc=#666666> | </fc>"                     -- Separators in xmobar
                                          , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                                          }
          }
          `removeKeysP` myKeysToRemove
          `additionalKeysP` myAdditionalKeys
