------------------------------------------------------------------------
 -------------------------------------------------------------------------------
--                  __  ____  __                       _                     --
--                  \ \/ /  \/  | ___  _ __   __ _  __| |                    --
--                   \  /| |\/| |/ _ \| '_ \ / _` |/ _` |                    --
--                   /  \| |  | | (_) | | | | (_| | (_| |                    --
--                  /_/\_\_|  |_|\___/|_| |_|\__,_|\__,_|                    --
--                                                                           --
-------------------------------------------------------------------------------
import Control.Monad (liftM2)
    -- Base
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import Control.Arrow (first)

    -- Data
import Data.Char (isSpace)
import Data.List
import Data.Monoid
import Data.Maybe (isJust)
import Data.Maybe (fromJust)
import qualified Data.Map as M

    -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad as NS
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.ClickableWorkspaces

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, isDialog)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.

    -- Actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies, copyToAll)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen, prevWS, nextWS)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

    -- Layouts modifiers
import XMonad.Layout.Accordion
import XMonad.Layout.Decoration
import XMonad.Layout.Fullscreen
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral 
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

    -- Layouts
import XMonad.Layout.Grid
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns

import Data.Semigroup
import XMonad.Hooks.DynamicProperty

import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ScreenCorners
import XMonad.Hooks.ManageDocks
------------------------------------------------------------------------
-- VARIABLES
------------------------------------------------------------------------
myFont :: [Char]
myFont = "xft:JetBrainsMono Nerd Font:Regular:pixelsize=12"

myModMask :: KeyMask
myModMask = mod4Mask       -- Sets modkey to super/windows key

myTerminal :: [Char]
myTerminal = "st"   -- Sets default terminal

myBorderWidth :: Dimension
myBorderWidth = 3          -- Sets border width for windows

myNormColor :: [Char]
myNormColor   = "#909090"  -- Border color of normal windows

myFocusColor :: [Char]
--myFocusColor  = "#0000FF"  -- Border color of focused windows
myFocusColor    = "#744700"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False
 

altMask :: KeyMask
altMask = mod4Mask         -- Setting this for use in xprompts

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

------------------------------------------------------------------------
-- AUTOSTART
------------------------------------------------------------------------
keyboardLayout = "setxkbmap -layout us,pl,ru,ua -option grp:alt_shift_toggle"

typingRepeatSpeed = "xset r rate 180 40"

cursor = "xsetroot -cursor_name left_ptr"

wallpapers = "~/.fehbg &"

notificationDaemon = "dunst"

pipewire = "~/pipewire.sh &"

tapping = "xinput set-prop 15 341 1"

myStartupHook :: X ()
myStartupHook = do
          --spawn keyboardLayout
          spawn typingRepeatSpeed
          spawn cursor
          --addScreenCorners [ (SCUpperLeft,namedScratchpadAction scratchpads "term")
                       -- , (SCUpperRight, goToSelected $ mygridConfig' myColorizer)
                       -- ,  (SCLowerLeft,  prevWS)
                       -- , (SCLowerRight, nextWS)
                        
          --            ]
          spawn wallpapers
          spawn notificationDaemon
          --spawnOnce pipewire
          spawnOnce tapping
         -- spawnOnce "nm-applet &"
          --spawn "eww open bar"
          spawn "picom &"
         -- spawn "mpd &"
          --spawn "xbacklight -set 20 &"
          spawn "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &"
          spawnOnce "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype request  --transparent true --alpha 255 --tint 0x000000  --height 20 --monitor 0 --iconspacing 1 &"
          spawnOnce "xmonad --recompile && xmonad --restart"


------------------------------------------------------------------------
-- XPROMPT SETTINGS
------------------------------------------------------------------------
dtXPConfig :: XPConfig
dtXPConfig = def
      { font                = "xft:Liberation Mono:size=9"
      , bgColor             = "white"
      , fgColor             = "#704214"
      , bgHLight            = "#704214"
      , fgHLight            = "white"
      , borderColor         = "#704214"
      , promptBorderWidth   = 3
--      , promptKeymap        = dtXPKeymap
     , position            = Bottom
--      , position            = Top
--      , position            = CenteredAt { xpCenterY = 0.3, xpWidth = 0.3 }
      , height              = 20
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Just 100000  -- set Just 100000 for .1 sec
      , showCompletionOnTab = False
      , searchPredicate     = isPrefixOf
      , alwaysHighlight     = True
      , maxComplRows        = Nothing      -- set to Just 5 for 5 rows
      }

-- The same config minus the autocomplete feature which is annoying on
-- certain Xprompts, like the search engine prompts.
dtXPConfig' :: XPConfig
dtXPConfig' = dtXPConfig
      { autoComplete = Nothing
      }

-- A list of all of the standard Xmonad prompts
promptList :: [(String, XPConfig -> X ())]
promptList = [ ("m", manPrompt)          -- manpages prompt
             , ("p", passPrompt)         -- get passwords (requires 'pass')
             , ("g", passGeneratePrompt) -- generate passwords (requires 'pass')
             , ("r", passRemovePrompt)   -- remove passwords (requires 'pass')
             , ("s", sshPrompt)          -- ssh prompt
             , ("x", xmonadPrompt)       -- xmonad prompt
             ]

------------------------------------------------------------------------
-- KEYBINDINGS
------------------------------------------------------------------------
-- I am using the Xmonad.Util.EZConfig module which allows keybindings
-- to be written in simpler, emacs-like format.
myKeys :: [([Char], X ())]
myKeys =
    -- Xmonad
        [ ("M-9",   spawn "xmonad --recompile && xmonad --restart" `withNotification` Message Normal "XMonad" "Recompiled and restarted!")
        , ("M-C-r", spawn "xmonad --recompile")      -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")        -- Restarts xmonad
        , ("M-0",                           spawn "~/Scripts/power.sh" )

    
        , ("M-S-<Return>", spawn (myTerminal))     -- launch terminal -- Open my main terminal
        , ("M-1", namedScratchpadAction scratchpads "term")
        , ("M-2", namedScratchpadAction scratchpads "pavucontrol")
        , ("M-3", namedScratchpadAction scratchpads "mpv")
        , ("M-4", namedScratchpadAction scratchpads "jamesdsp")
        , ("M-5", namedScratchpadAction scratchpads "telegram-desktop")
        , ("M-6", namedScratchpadAction scratchpads "spotify")
        , ("M-7", namedScratchpadAction scratchpads "gcolor3")
    -- Youtube launcher
        , ("M-y", spawn (myTerminal ++ " -e ytfzf -tf" ))                 -- Youtube fzf 
        , ("M-S-y", spawn (myTerminal ++ " -e /home/mhabibmanan/.local/bin/ytui_music run" )) --YTMusic
    -- Run Prompts
      --  , ("M-<Return>", shellPrompt dtXPConfig)   -- Shell Prompt
        , ("M-p", spawn "dmenu_run -l 5 -c -fn 'JetBrainsMono NFM-11' -nf '#744700' -nb white -sb '#744700' -sf white")    -- dmenu
        , ("M-S-p", spawn "j4-dmenu-desktop") --j4-dmenu-desktop
        , ("S-<Tab>",                         spawn "rofi -show window -theme android_notification -show-icons")

    -- Windows
        , ("M-S-c", kill1)                           -- Kill the currently focused client
       -- , ("M-S-a", killAll)                         -- Kill all windows on current workspace

    -- Floating windows
        , ("M-f", sendMessage (T.Toggle "Flo"))       -- Toggles my 'floats' layout
        , ("M-<Delete>", withFocused $ windows . W.sink) -- Push floating window back to tile
        , ("M-S-<Delete>", sinkAll)                      -- Push ALL floating windows to tile


    -- Windows navigation
        , ("M-m", windows W.focusMaster)     -- Move focus to the master window
        , ("M-j", windows W.focusDown)       -- Move focus to the next window
        , ("M-k", windows W.focusUp)         -- Move focus to the prev window
        , ("M-S-m", windows W.swapMaster)    -- Swap the focused window and the master window
        , ("M-S-j", windows W.swapDown)      -- Swap focused window with next window
        , ("M-S-k", windows W.swapUp)        -- Swap focused window with prev window
        , ("M-<Backspace>", promote)         -- Moves focused window to master, others maintain order
        , ("M1-S-<Tab>", rotSlavesDown)      -- Rotate all windows except master and keep focus in place
        , ("M1-C-<Tab>", rotAllDown)         -- Rotate all the windows in the current stack
        --, ("M-S-s", windows copyToAll)  
        , ("M-C-s", killAllOtherCopies) 
        
        , ("M-C-M1-<Up>", sendMessage Arrange)
        , ("M-C-M1-<Down>", sendMessage DeArrange)
    -- Works in Float mode
        , ("M-<Up>", sendMessage (MoveUp 10))             --  Move focused window to up
        , ("M-<Down>", sendMessage (MoveDown 10))         --  Move focused window to down
        , ("M-<Right>", sendMessage (MoveRight 10))       --  Move focused window to right
        , ("M-<Left>", sendMessage (MoveLeft 10))         --  Move focused window to left
        , ("M-S-<Up>", sendMessage (IncreaseUp 10))       --  Increase size of focused window up
        , ("M-S-<Down>", sendMessage (IncreaseDown 10))   --  Increase size of focused window down
        , ("M-S-<Right>", sendMessage (IncreaseRight 10)) --  Increase size of focused window right
        , ("M-S-<Left>", sendMessage (IncreaseLeft 10))   --  Increase size of focused window left
        , ("M-C-<Up>", sendMessage (DecreaseUp 10))       --  Decrease size of focused window up
        , ("M-C-<Down>", sendMessage (DecreaseDown 10))   --  Decrease size of focused window down
        , ("M-C-<Right>", sendMessage (DecreaseRight 10)) --  Decrease size of focused window right
        , ("M-C-<Left>", sendMessage (DecreaseLeft 10))   --  Decrease size of focused window left

    -- Layouts
        , ("M-<Tab>", sendMessage NextLayout)                                    -- Switch to next layout
        , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
        , ("M-S-<Space>", sendMessage ToggleStruts)                              -- Toggles struts
        , ("M-S-n", sendMessage $ MT.Toggle NOBORDERS)                              -- Toggles noborder
        , ("M-<KP_Multiply>", sendMessage (IncMasterN 1))   -- Increase number of clients in master pane
        , ("M-<KP_Divide>", sendMessage (IncMasterN (-1)))  -- Decrease number of clients in master pane
        , ("M-S-<KP_Multiply>", increaseLimit)              -- Increase number of windows
        , ("M-S-<KP_Divide>", decreaseLimit)                -- Decrease number of windows

        , ("M-h", sendMessage Shrink)                       -- Shrink horiz window width
        , ("M-l", sendMessage Expand)                       -- Expand horiz window width
        , ("M-C-j", sendMessage MirrorShrink)               -- Shrink vert window width
        , ("M-C-k", sendMessage MirrorExpand)               -- Exoand vert window width


    -- Workspaces
        , ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP)       -- Shifts focused window to next ws
        , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP),  -- Shifts focused window to prev ws
        ("M-q",                               windows $ W.greedyView "q"),
        ("M-w",                               windows $ W.greedyView "w"),
        ("M-e",                               windows $ W.greedyView "e"),
        ("M-a",                               windows $ W.greedyView "a"),
        ("M-s",                               windows $ W.greedyView "s"),
        ("M-d",                               windows $ W.greedyView "d"),
        ("M-S-q",                             windows $ W.shift "q"),
        ("M-S-w",                             windows $ W.shift "w"),
        ("M-S-e",                             windows $ W.shift "e"),
        ("M-S-a",                             windows $ W.shift "a"),
        ("M-S-s",                             windows $ W.shift "s"),
        ("M-S-d",                             windows $ W.shift "d"),
        ("M-b",                               sendMessage ToggleStruts)
       
        , ("M-8",                             windows copyToAll)     -- Make window appears in all ws
        , ("M-S-8",                           killAllOtherCopies)    -- Disable universal window

    -- Multimedia Keys
        , ("<XF86AudioPlay>", spawn "cmus toggle")
        , ("<XF86AudioPrev>", spawn "cmus prev")
        , ("<XF86AudioNext>", spawn "cmus next")
        , ("<XF86AudioMute>",   spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
        , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%" `withNotification` Command Low "Vol" "light")
        , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%" `withNotification` Command Low "Vol" "light")
        
        , ("<XF86MonBrightnessDown>",           spawn "light -U 10" `withNotification` Command Low "Bright" "light" )
        , ("<XF86MonBrightnessUp>",             spawn "light -A 10" `withNotification` Command Low "Bright" "light" )
        , ("<Print>",                         spawn "~/Scripts/scmenu")
        , ("M-S-<Print>",                       spawn "~/Scripts/dmenurecord")
        , ("M-C-<Print>",                       spawn "~/Scripts/dmenu-record")
	, ("M-C-g",                       spawn "~/Scripts/google-search.sh") 
        --, ("<Print>",         spawn "scrot"  `withNotification` Command Critical "Saved" "")
        , ("M-<Print>",  spawn "~/Scripts/ss.sh")
        ]
        -- Appending search engines to keybindings list
        ++ [("M-S-<Tab>" ++ k, f dtXPConfig') | (k,f) <- promptList ]
       -- ++ [("M-p " ++ k, f dtXPConfig' g) | (k,f,g) <- promptList' ]
        -- Appending named scratchpads to keybindings list
          where nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))
                
-- Send notification
data UrgencyLevel = Low | Normal | Critical

instance Show UrgencyLevel where
  show Low = "low"
  show Normal = "normal"
  show Critical = "critical"

data Notification
  = Message UrgencyLevel String String
  | Command UrgencyLevel String String

wrapInQuotes, wrapIntoCommand :: String -> String
wrapInQuotes = wrap "'" "'"
wrapIntoCommand = wrap "$(" ")"

sendNotification :: Notification -> X ()
sendNotification (Message uLevel summary body) = spawn ("notify-send " ++ wrapInQuotes summary ++ " " ++ wrapInQuotes body ++ " -u " ++ wrapInQuotes (show uLevel))
sendNotification (Command uLevel summary body) = spawn ("notify-send " ++ wrapInQuotes summary ++ " " ++ wrapIntoCommand body ++ " -u " ++ wrapInQuotes (show uLevel))

withNotification :: X () -> Notification -> X ()
withNotification action notification = action >> sendNotification notification

------------------------------------------------------------------------
-- WORKSPACES
------------------------------------------------------------------------


xmobarEscape :: [Char] -> [Char]
xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]
        
myWorkspaces :: [String]   
myWorkspaces = ["q","w","e","a","s","d"]
ignoredWorkspaces = ["NSP"]

------------------------------------------------------------------------
-- MANAGEHOOK
------------------------------------------------------------------------
-- Sets some rules for certain programs. Examples include forcing certain
-- programs to always float, or to always appear on a certain workspace.
-- Forcing programs to a certain workspace with a doShift requires xdotool
-- if you are using clickable workspaces. You need the className or title 
-- of the program. Use xprop to get this info.

-- Window rules
rectCentered :: Rational -> W.RationalRect
rectCentered percentage = W.RationalRect offset offset percentage percentage
  where
    offset = (1 - percentage) / 2

vertRectCentered :: Rational -> W.RationalRect
vertRectCentered height = W.RationalRect offsetX offsetY width height
  where
    width = height / 2
    offsetX = (1 - width) / 2
    offsetY = (1 - height) / 2

viewShift :: WorkspaceId -> Query (Endo WindowSet)
viewShift = doF . liftM2 (.) W.greedyView W.shift

htopWindowQuery :: Query Bool
htopWindowQuery = title =? "HTOP"

pulseMixerWindowQuery :: Query Bool
pulseMixerWindowQuery = title =? "PulseMixer"

rangerWindowQuery :: Query Bool
rangerWindowQuery = title =? "Ranger"

zoomWindowQuery :: Query Bool
zoomWindowQuery = className =? "zoom"

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [ 
--classname
       className =? "Gimp"                           --> doShift ( myWorkspaces !! 8)
     , className =? "Gimp"                           --> doFloat
     , className =? "Xdm-app"                        --> doFloat 
     , className =? "Arandr"                         --> customFloating (rectCentered 0.6)
     , className =? "Spotify"                            --> customFloating (rectCentered 0.6)
     , className =? "mpv"                            --> customFloating (rectCentered 0.6)
     , className =? "firefox"                        --> viewShift "w"
     , className =? "qutebrowser"                    --> viewShift "w"
     , className =? "Xdm-app"                        --> viewShift "e"
     , className =? "Virt-manager"                   --> viewShift "d"
--titles 
     , title     =? "Media viewer"                   --> doFloat
     , title     =? "Picture-in-Picture"             --> customFloating (rectCentered 0.5)
     , title     =? "File Upload"                    --> customFloating (rectCentered 0.8)
--resources 
     , resource  =? "desktop_window"                 --> doIgnore
     , resource  =? "kdesktop"                       --> doIgnore
     , isDialog --> doFloat
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     ]

------------------------------------------------------------------------
-- LOGHOOK
------------------------------------------------------------------------
-- Sets opacity for inactive (unfocused) windows. I prefer to not use
-- this feature so I've set opacity to 1.0. If you want opacity, set
-- this to a value of less than 1 (such as 0.9 for 90% opacity).
myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 0.0

------------------------------------------------------------------------
-- LAYOUTS
------------------------------------------------------------------------
defaultTall   = Tall 1 0.05 0.5

defaultLayout = renamed [Replace "Def"] 
                $ limitWindows 4 
                $ defaultSpacing defaultTall

resizable     = renamed [Replace "Res"] 
                $ limitWindows 4 
                $ defaultSpacing 
                $ ResizableTall 1 0.05 0.5 []

tabbedLayout  = renamed [Replace "Tab"] 
                $ noBorders 
                $ smartBorders . avoidStruts
                $ tabbedBottom shrinkText myTabbedTheme

myTabbedTheme =
  def
    { fontName = myFont,
      activeColor = "#744700",
      inactiveColor = "#eeeeee",
      activeBorderColor = "#744700",
      inactiveBorderColor = "#744700",
      activeTextColor = "#eeeeee",
      inactiveTextColor = "#744700"
    }

mirrorLayout  = renamed [Replace "Mir"] 
                $ defaultSpacing 
                $ Mirror defaultTall

gridLayout    = renamed [Replace "Gri"] 
                -- $ smartBorders . avoidStruts
                $ defaultSpacing Grid

monocleLayout = renamed [Replace "Mon"] 
                $ noBorders Full

floats        = renamed [Replace "Flo"]
               $ smartBorders 
               $ simplestFloat


myLayoutHook = screenCornerLayoutHook 
               $ onWorkspace "w" (tabbedLayout ||| monocleLayout ||| gridLayout )
               $ onWorkspace "d" (tabbedLayout ||| monocleLayout)
               $ smartBorders . avoidStruts $ gridLayout ||| monocleLayout ||| resizable ||| tabbedLayout ||| mirrorLayout ||| floats

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

defaultSpacing :: l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
defaultSpacing = mySpacing 1

scratchpads :: [NamedScratchpad]
scratchpads = [
-- run htop in xterm, find it by title, use default floating window placement
    NS "mpv" "mpv" (className =? "mpv")
        (customFloating $ rectCentered 0.5),

    NS "term" "st -n scratchpad" (resource =? "scratchpad")
        (customFloating $ rectCentered 0.7),

    NS "pavucontrol" "pavucontrol" (className =? "Pavucontrol")
        (customFloating $ W.RationalRect (1/4) (1/4) (2/4) (2/4)),

    NS "jamesdsp" "jamesdsp" (className =? "jamesdsp")
        (customFloating $ rectCentered 0.8),

    NS "telegram-desktop" "telegram-desktop" (className =? "TelegramDesktop")
        (customFloating $ rectCentered 0.95),
    
    NS "gcolor3" "gcolor3" (className =? "Xdm-app")
        (customFloating $ W.RationalRect (1/4) (1/4) (2/4) (2/4)),


    NS "spotify" "env LD_PRELOAD=/usr/lib/spotify-adblock.so spotify %U" (className =? "Spotify")
        (customFloating $  W.RationalRect (0.1)(0.1)(0.8)(0.8))   
  ]

myHandleEventHook :: Event -> X All
myHandleEventHook = dynamicPropertyChange "WM_NAME" (title =? "Spotify" --> floating)
        where floating = customFloating $ rectCentered 0.8
------------------------------------------------------------------------
-- MAIN
------------------------------------------------------------------------
main :: IO ()
main = do
    -- Launching xmobar
    xmproc0 <- spawnPipe "xmobar -x 0 /home/mhabibmanan/.xmobarrc"
    --xmproc1 <- spawnPipe "xmobar -x 0 /home/mhabibmanan/.config/xmobar/.xmobarrc2"
    -- the xmonad, ya know...what the WM is named after!
    xmonad $ docks def
    --xmonad $ ewmh def
        { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks <+> namedScratchpadManageHook scratchpads
        , handleEventHook    = myHandleEventHook
                             
                               <+> serverModeEventHookCmd 
                               <+> serverModeEventHook 
                               <+> screenCornerEventHook 
                               <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = avoidStruts $ myLayoutHook 
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook = dynamicLogWithPP $ filterOutWsPP [scratchpadWorkspaceTag] $ xmobarPP
                        { ppOutput = \x -> hPutStrLn xmproc0 x                -- >> hPutStrLn xmproc1 x  >> hPutStrLn xmproc2 x
                        , ppCurrent = xmobarColor "#704214" "" . wrap "[" "]" -- Current workspace in xmobar
                        , ppVisible = xmobarColor "#c3e88d" "" . wrap " " " "      -- Visible but not current workspace
                        , ppHidden = xmobarColor "#c86a13" "" . wrap "*" " "  -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor "#666666" "" . wrap " " " "      -- Hidden workspaces (no windows)
                        , ppTitle = xmobarColor "#704214" "" . shorten 40     -- Title of active window in xmobar
                        , ppSep =  "<fc=#704214> | </fc>"                     -- Separators in xmobar
                        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                        , ppExtras  = [windowCount]                           -- # of windows current workspace
                    
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        }
        } `additionalKeysP` myKeys
