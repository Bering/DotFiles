import System.Exit
import System.IO (hPutStrLn)
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.ResizableTile
import XMonad.Prompt
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Ssh
import XMonad.Util.EZConfig
import XMonad.Util.Paste
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce
import qualified XMonad.StackSet as W

-- TODO:
-- * XMonad.Util.Spotify or another way to make keyboard prev/next play/pause work
-- * touchpad tap to click and 2 fingers right-click

myTerminal = "alacritty"

myWorkspaces = map show [1..12]

myStartupHook = do
                  spawnOnce "deadd-notification-center &"
                  spawnOnce "feh --no-fehbg --bg-scale '/home/phil/Images/camo tech MSI.jpg'"
                  spawnOnce "xsetroot -cursor_name left_ptr"
                  spawnOnce "picom &"
                  spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint 0x292d3e --height 18 --iconspacing 3 &"
                  spawnOnce "nm-applet &"
                  spawnOnce "pasystray &"
                  spawnOnce "blueman-tray &"
                  spawnOnce "pamac-tray &"
                  spawnOnce "cbatticon &"

myLayout = (renamed [Replace "Left"] $ ResizableTall 1 (3/100) (1/2) [])
       ||| (renamed [Replace "Right"] $ reflectHoriz (ResizableTall 1 (3/100) (1/2) []))
       ||| (renamed [Replace "Up"] $ Mirror (ResizableTall 1 (3/100) (1/2) []))
       ||| (noBorders (Full))

promptConfig :: XPConfig
promptConfig = def
        { font = "xft:BitstreamVeraSansMono:size=10:bold:antialias=true"
        , position = Top
        , height = 20
        , bgColor = "black"
        , fgColor = "white"
        , bgHLight = "#66EE66"
        , fgHLight = "#666666"
        }

myKeysToRemove =   [ "M-S-<Return>"  -- terminal
                   , "M-p"           -- dmenu
                   , "M-S-p"         -- gmrun
                   , "M-S-c"         -- kill
                   , "M-S-q"         -- quit
                   , "M-q"           -- recompile and restart
                   , "M-j"           -- focusDown
                   , "M-k"           -- focusUp
                   , "M-S-j"         -- swapDown
                   , "M-S-k"         -- swapUp
                   , "M-h"           -- Shrink
                   , "M-l"           -- Expand
                   , "M-,"           -- increase number of clients in master
                   , "M-."           -- decrease number of clients in master
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
                   -- screens
                   , "M-w", "M-e", "M-r"
                   ]

myAdditionalKeys = [ ("M-r l",                      spawn "slock")
                   , ("M-r r",                      restart "xmonad" True)
                   , ("M-r x",                      io exitSuccess)
                   , ("M-r b",                      spawn "sudo shutdown -r now")
                   , ("M-r h",                      spawn "sudo shutdown now")
                   -- navigation
                   , ("M-<Up>",                     windows W.focusUp)
                   , ("M-<Down>",                   windows W.focusDown)
                   , ("M-<Left>",                   prevScreen)
                   , ("M-<Right>",                  nextScreen)
                   -- move windows around
                   , ("M-S-<Up>",                   windows W.swapUp)
                   , ("M-S-<Down>",                 windows W.swapDown)
                   , ("M-S-<Left>",                 shiftPrevScreen)
                   , ("M-S-<Right>",                shiftNextScreen)
                   , ("M-<Page_Down>",              withFocused $ windows.W.sink)
                   -- layout manipulation
                   , ("M-C-<Left>",                 sendMessage Shrink)
                   , ("M-C-<Right>",                sendMessage Expand)
                   , ("M-C-<Down>",                 sendMessage MirrorShrink)
                   , ("M-C-<Up>",                   sendMessage MirrorExpand)
                   , ("M-<KP_Add>",                 sendMessage (IncMasterN 1))
                   , ("M-<KP_Subtract>",            sendMessage (IncMasterN (-1)))
                   , ("M-s",                        sendMessage ToggleStruts)
                   -- applications
                   , ("M-p",                        shellPrompt promptConfig)
                   , ("M-S-p",                      sshPrompt promptConfig)
                   , ("M-n",                        spawn "kill -s USR1 $(pidof deadd-notification-center)")
                   , ("M-1",                        spawn myTerminal)
                   , ("M-2",                        spawn "nautilus")
                   , ("M-3",                        spawn "firefox")
                   , ("M-4",                        spawn "steam")
                   -- misc
                   , ("M-w",                        kill)
                   , ("M-v",                        pasteSelection)
                   , ("M-l",                        spawn "slock")
                   , ("<XF86ModeLock>",             spawn "slock")
                   , ("<XF86HomePage>",             spawn "nautilus")
                   , ("<XF86Calculator>",           spawn "gnome-calculator")
                   , ("<XF86AudioMute>",            spawn "amixer -D pulse set Master 1+ toggle")
                   , ("<XF86AudioLowerVolume>",     spawn "amixer set Master 5%- unmute")
                   , ("<XF86AudioRaiseVolume>",     spawn "amixer set Master 5%+ unmute")
                   , ("<Print>",                    spawn "gnome-screenshot --interactive")
                   ]
                   -- workspaces (f1 - f12)
                   ++
                   [ ("M-" ++ otherModMasks ++ key, action tag)
                     | (tag, key)  <- zip myWorkspaces (map (\x -> "<F" ++ show x ++ ">") [1..12])
                     , (otherModMasks, action) <- [ ("", windows . W.greedyView) -- or W.view
                                                  , ("S-", windows . W.shift)]
                   ]

myManageHook = composeAll
                   [ className =? "Gnome-calculator"        --> doFloat
                   , className =? "Gnome-screenshot"        --> doFloat
                   ]

main = do
        xmproc0 <- spawnPipe "xmobar -x 0 /home/phil/.config/xmobar/xmobarrc0"
        xmproc1 <- spawnPipe "xmobar -x 1 /home/phil/.config/xmobar/xmobarrc1"
        xmonad $ ewmh def
          { terminal = myTerminal
          , modMask = mod4Mask
          , borderWidth = 2
          , workspaces = myWorkspaces
          , startupHook = myStartupHook
          , layoutHook = avoidStruts $ myLayout
          , manageHook = myManageHook <+> manageDocks
          , handleEventHook = docksEventHook
          , logHook = dynamicLogWithPP xmobarPP
                                          { ppOutput  = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x
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
