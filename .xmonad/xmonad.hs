import System.Exit
import System.IO (hPutStrLn)
import XMonad hiding ( (|||)) -- don't use the normal ||| operator, use the one provided in LayoutCombinators instead
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowNavigation
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Accordion
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig
import XMonad.Util.Paste
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce
import qualified XMonad.StackSet as W

myTerminal = "alacritty"

myStartupHook = do
                  spawnOnce "deadd-notification-center &"
                  spawnOnce "~/Scripts/monitors.setup.dual.sh"
                  spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &"
                  spawnOnce "feh --no-fehbg --bg-scale '/home/phil/Images/camo tech manjaro.jpg'"
                  spawnOnce "xsetroot -cursor_name left_ptr"
                  spawnOnce "picom --experimental-backends &"
                  spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint 0x000000 --height 18 --iconspacing 3 &"
                  spawnOnce "nm-applet &"
                  spawnOnce "pasystray &"
                  spawnOnce "blueman-tray &"
                  spawnOnce "pamac-tray &"
                  spawnOnce "cbatticon &"
                  spawnOnce "redshift-gtk &"
                  spawnOnce "udiskie -a -s &"
                  spawnOnce "optimus-manager-qt &"

myWorkspaces = ["\xf015", "2", "3", "4", "\xf269", "6", "7", "8", "\xf1b6", "10", "11", "\xf1bc"]

myProjects :: [Project]
myProjects = 
        [ Project { projectName      = "\xf269"
                  , projectDirectory = "~/"
                  , projectStartHook = Just $ do spawn "firefox"
                  }
        , Project { projectName      = "\xf1b6"
                  , projectDirectory = "~/"
                  , projectStartHook = Just $ do spawn "steam"
                  }
        , Project { projectName      = "\xf1bc"
                  , projectDirectory = "~/"
                  , projectStartHook = Just $ do spawn "spotify"
                  }
        ]

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
                   , "M-<Return>"    -- swapMain
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

myAdditionalKeys = [ ("M-s l",                      spawn "dm-tool lock")
                   , ("M-s r",                      restart "xmonad" True)
                   , ("M-s x",                      io exitSuccess)
                   , ("M-s b",                      spawn "sudo shutdown -r now")
                   , ("M-s h",                      spawn "sudo shutdown now")
                   -- layout manipulation
                   , ("M-C-<Left>",                 sendMessage Shrink)
                   , ("M-C-<Right>",                sendMessage Expand)
                   , ("M-C-<Down>",                 sendMessage MirrorShrink)
                   , ("M-C-<Up>",                   sendMessage MirrorExpand)
                   , ("M-S-<Return>",               promote)
                   , ("M-<Page_Up>",                rotSlavesUp)
                   , ("M-<Page_Down>",              rotSlavesDown)
                   , ("M-<KP_Add>",                 sendMessage (IncMasterN 1))
                   , ("M-<KP_Subtract>",            sendMessage (IncMasterN (-1)))
                   , ("M-<End>",                    withFocused $ windows . W.sink)
                   , ("M-b",                        sendMessage ToggleStruts) -- a.k.a. toggle bars
                   -- layout selection
                   , ("M-<Space>",                  sendMessage NextLayout)
                   , ("M-l l",                      sendMessage $ JumpToLayout "Left")
                   , ("M-l r",                      sendMessage $ JumpToLayout "Right")
                   , ("M-l u",                      sendMessage $ JumpToLayout "Up")
                   , ("M-l a",                      sendMessage $ JumpToLayout "Accordion")
                   , ("M-l g",                      sendMessage $ JumpToLayout "Grid")
                   , ("M-l f",                      sendMessage $ JumpToLayout "Full")
                   -- applications
                   , ("M-p",                        spawn "rofi -show drun -show-icons")
                   , ("M-S-p",                      spawn "rofi -show run")
                   , ("M-M1-p",                     spawn "rofi -show ssh")
                   , ("M-C-p",                      spawn "Scripts/pass-qr.sh")
                   , ("M-n",                        spawn "kill -s USR1 $(pidof deadd-notification-center)")
                   , ("M-<Return>",                 spawn myTerminal)
                   , ("M-1",                        spawn myTerminal)
                   , ("M-2",                        spawn "nautilus")
                   , ("M-3",                        spawn "firefox")
                   , ("M-4",                        spawn "steam")
                   -- misc
                   , ("M-w",                        kill)
                   , ("M-k",                        spawn "dm-tool lock")
                   , ("<XF86ModeLock>",             spawn "dm-tool lock")
                   , ("<XF86HomePage>",             spawn "nautilus")
                   , ("<XF86Calculator>",           spawn "gnome-calculator")
                   , ("<XF86AudioMute>",            spawn "amixer -D pulse set Master 1+ toggle")
                   , ("<XF86AudioLowerVolume>",     spawn "amixer set Master 5%- unmute")
                   , ("<XF86AudioRaiseVolume>",     spawn "amixer set Master 5%+ unmute")
                   , ("<XF86AudioPlay>",            spawn "playerctl play-pause")
                   , ("<XF86AudioPrev>",            spawn "playerctl previous")
                   , ("<XF86AudioNext>",            spawn "playerctl next")
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
                   [ className =? "Gnome-calculator"      --> doCenterFloat
                   , className =? "Gnome-screenshot"      --> doCenterFloat
                   , className =? "Dragon-drag-and-drop"  --> doCenterFloat
                   , title     =? "Steam Login"           --> doCenterFloat
                   , title     =? "Steam Guard - Computer Authorization Required" --> doCenterFloat
                   ]

layoutLeft      = renamed [Replace "Left"]
                $ avoidStruts
                $ spacingRaw True (Border 0 0 0 0) False (Border 1 1 1 1) True
                $ ResizableTall 1 (3/100) (3/5) []
layoutRight     = renamed [Replace "Right"]
                $ avoidStruts
                $ spacingRaw True (Border 0 0 0 0) False (Border 1 1 1 1) True
                $ reflectHoriz (ResizableTall 1 (3/100) (3/5) [])
layoutUp        = renamed [Replace "Up"]
                $ avoidStruts
                $ spacingRaw True (Border 0 0 0 0) False (Border 1 1 1 1) True
                $ Mirror (ResizableTall 1 (3/100) (3/5) [])
layoutAccordion = renamed [Replace "Accordion"]
                $ avoidStruts
                $ spacingRaw True (Border 0 0 0 0) False (Border 1 1 1 1) True
                $ Accordion
layoutGrid      = renamed [Replace "Grid"]
                $ avoidStruts
                $ spacingRaw True (Border 0 0 0 0) False (Border 1 1 1 1) True
                $ Grid
layoutFull      = noBorders(Full)
myLayout        = layoutLeft ||| layoutRight ||| layoutUp ||| layoutAccordion ||| layoutGrid ||| layoutFull

main = do
        xmproc0 <- spawnPipe "xmobar -x 0 /home/phil/.config/xmobar/xmobarrc0"
        xmproc1 <- spawnPipe "xmobar -x 1 /home/phil/.config/xmobar/xmobarrc1"
        config <- withWindowNavigation (xK_Up, xK_Left, xK_Down, xK_Right)
                $ ewmh
                $ fullscreenSupport
                $ dynamicProjects myProjects def
          { terminal = myTerminal
          , modMask = mod4Mask
          , borderWidth = 2
          , workspaces = myWorkspaces
          , startupHook = myStartupHook
          , layoutHook = myLayout
          , manageHook = myManageHook <+> manageDocks
          , handleEventHook = docksEventHook
          , logHook = dynamicLogWithPP xmobarPP
                                          { ppOutput  = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x
                                          , ppSep =  " "                                      -- Separators in xmobar
                                          , ppUrgent = xmobarColor "red" "" . wrap "!" "!"    -- Urgent workspace
                                          , ppCurrent = xmobarColor "cyan" "" . wrap "[" "]"  -- Current workspace in xmobar
                                          , ppVisible = xmobarColor "cyan" ""                 -- Visible but not current workspace
                                          , ppHidden  = xmobarColor "orange" ""               -- Hidden workspaces in xmobar
                                          , ppHiddenNoWindows = xmobarColor "lightgreen" ""   -- Hidden workspaces (no windows)
                                          , ppLayout = wrap " <fc=#888888>\xf928</fc> <fc=white>" "</fc>"
                                          , ppOrder  = \(ws:l:t:ex) -> [ws,l]
                                          }
                        >> updatePointer (0.5, 0.5) (0, 0)
          }
          `removeKeysP` myKeysToRemove
          `additionalKeysP` myAdditionalKeys
        
        xmonad config
