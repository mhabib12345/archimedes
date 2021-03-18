---------------------------------------------------------------------
---------------------------------------------------------------------
import XMonad
import Data.Monoid
import System.Exit
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
--import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Util.SpawnOnce
import XMonad.Hooks.SetWMName
import XMonad.Util.NamedScratchpad
--import XMonad.Hooks.EwmhDesktops
--import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Spacing
--import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Fullscreen
--import Multimedia Keys
import Graphics.X11.ExtraTypes.XF86
----------------------------------------------------------------------
--                           USER VARIABLES                         --
----------------------------------------------------------------------

myTerminal :: String
myTerminal  = "termite"
my2ndTerminal :: String
my2ndTerminal  = "st"
myWBrowser :: String
myWBrowser  = "waterfox-g3"
myScreenShot :: String
myScreenShot  = "flameshot gui"
myFileManager :: String
myFileManager = "pcmanfm"
myLauncher :: String
myLauncher = "dmenu_run"


myWorkspaces = ["q:\xe065","w:\xe007","e:\xf07b","r:\xf044","t:\xf7d9","y:\xf108","a:\xf53f","s:\xf3fe","d:"]

----------------------------------------------------------------------
--                              KEYS                                --
----------------------------------------------------------------------
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- Log out
    [
 ((modm .|. controlMask, xK_q), io (exitWith ExitSuccess))
    -- Reload XMonad
    , ((modm, xK_0), spawn "xmonad --recompile; xmonad --restart")
    -- Capture a screenshot
    , ((0, xK_Print), spawn myScreenShot)
    ---------------------------
    -- Launcher
    , ((modm,  xK_p), spawn myLauncher)
    -- File browser
    , ((modm, xK_f), spawn myFileManager)
    , ((modm .|. shiftMask, xK_f), spawn "termite -e ranger")
    -- Web browser
    , ((modm, xK_b), spawn myWBrowser)
    -- Gimp
    , ((modm, xK_g), spawn "gimp")
    -- Music player
    , ((modm, xK_c), spawn "termite -e cmus")
    -- Terminal
    , ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm, xK_Return), spawn my2ndTerminal)
    -- Text editor
    -- , ((modm .|. shiftMask, xK_t), spawn myTEditor)
    --------------------------------------
    -- Switch layout
    , ((modm, xK_l ), sendMessage NextLayout)
    -- Switch to layout defaults
    , ((modm .|. shiftMask, xK_l ), setLayout $ XMonad.layoutHook conf)
    -- Change focus
    , ((modm, xK_Right), windows W.focusDown)
    , ((modm, xK_Tab), windows W.focusDown)
    -- Change focus
    , ((modm, xK_Left), windows W.focusUp)
    -- Swap windows
    , ((modm .|. shiftMask, xK_Left ), windows W.swapUp)
    -- Swap windows
    , ((modm .|. shiftMask, xK_Right ), windows W.swapDown)
    -- Jump to urgent
    -- not mapped yet
    -- Increase master window width
    , ((modm .|. controlMask, xK_Up), sendMessage Expand)
    -- Decrease master window width
    , ((modm .|. controlMask, xK_Down), sendMessage Shrink)
    -- increase / decrease current slave window size (from resizabletile)
    , ((modm .|. controlMask, xK_Right), sendMessage MirrorShrink)
    -- increase / decrease current slave window size (from resizabletile)
    , ((modm .|. controlMask, xK_Left), sendMessage MirrorExpand)
    -- Toggle struts
    , ((modm .|. controlMask, xK_space), sendMessage ToggleStruts)
    -- Kill window
    , ((modm .|. shiftMask, xK_c), kill)
    -- Sink a floating window back into tiling
    , ((modm .|. shiftMask, xK_space), withFocused $ windows . W.sink)
    -- Move focused client to master
    , ((modm .|. shiftMask, xK_m), windows W.focusMaster)

    -- scratchpads
    -- bound to number keys
    , ((modm, xK_1), namedScratchpadAction myScratchPads "term1")
    , ((modm, xK_2), namedScratchpadAction myScratchPads "term2")
    , ((modm, xK_3), namedScratchpadAction myScratchPads "term3")
    --Multimedia Keys
    , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")        -- volume keys
    , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%") -- volume keys
    , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%") -- volume keys:
    ----------------------------------------------------------
    -- most of these keybinds will cause conflicts. if you  --
    -- want to use them, remap them. i've put them to       --
    -- one side for now                                     --
    ----------------------------------------------------------
    --
    -- -- Resize viewed windows to the correct size
    -- , ((modm,               xK_n     ), refresh)
    -- -- Move focus to the master window
    -- , ((modm,               xK_m     ), windows W.focusMaster  )
    -- -- Swap the focused window and the master window
    -- , ((modm,               xK_Return), windows W.swapMaster)
    -- -- Increment the number of windows in the master area
    -- , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    -- -- Deincrement the number of windows in the master area
    -- , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    -- -- Run xmessage with a summary of the default keybindings (useful for beginners)
    -- , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    -----------------------------------------------------------------------------------------------

    ]
    ++

    --------------------------------------
    -- ??bind workspaces to mod + 1-9?? --
    --------------------------------------
    -- because i like having my workspaces bound to asdzxcv keys
    -- i've commented out this bit (which is a fiarly standard
    -- setting in all tiling WMs) so edit them back in if you like
    -- and delete the almost identical replica
    ---------------------------------------------------------------
    -- [((m .|. modm, k), windows $ f i)
    --     | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    --     , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- ++
    ---------------------------------------------------------------

    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_q, xK_w, xK_e, xK_r, xK_t, xK_y, xK_a, xK_s, xK_d ]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [] [0..]
       , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


----------------------------------------------------------------------
-- LOOK AND FEEL                                                    --
----------------------------------------------------------------------

-- the next two lines just create the spacing. ripped from old tech bloke's
-- config. to actually set the size of the gaps use mySpacing in the layout
-- section.

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = True

myBorderWidth :: Dimension
myBorderWidth = 2

myNormalBorderColor  = "#05070c"
myFocusedBorderColor = "#5294e2"


---------------------------------------------------------------------
-- LAYOUTS                                                          --
----------------------------------------------------------------------

-- i only really use two layouts, and one of them is the first
-- flipped on its side. resizable tall uses the mirror expand/shrink
-- to resize the slave windows, which unlike the master, must have
-- focus when resizing. also they work differently depending on which
-- of the slave windows you are resizing.
--


myLayout =
    avoidStruts monadtall
    ||| avoidStruts monadwide
    ||| sfloat
    ||| full

  where
     monadtall =
        renamed [Replace "Monad Tall"]
        -- $ fullscreenFull
        $ smartBorders
        $ mySpacing 2   -- sets the gap size in px
        $ ResizableTall 1 (10/100) (1/2) []

     -- this layout takes all the values from previous
     -- layout and flips by 90°
     monadwide =
        renamed [Replace "Monad Wide"]
        $ Mirror monadtall

     full =
        renamed [Replace "Full Screen"]
        $ noBorders
        $ Full

     sfloat =
        renamed [Replace "Floating"]
        $ simplestFloat


     -- this isn't quite how i'd like it. i'd prefer gaps to act like
     -- smart boarders, only appearing if there's more than one window.
     -- there's probably a way of doing it. must find out how.

-----------------------------------------------------------------------
-- WINDOW RULES
-----------------------------------------------------------------------

-- i've added some hooks to make certain windows spawn below the
-- master window, rather than replacing it. at first i wanted
-- to work like every other WM and do this by default but actually
-- this is better. there is a way of doing that, see the FAQ
-- https://wiki.haskell.org/Xmonad/Frequently_asked_questions#Make_new_windows_appear_.27below.27_rather_than_.27above.27_the_current_window

myManageHook = composeAll
    [ className =? "MPlayer"                        --> doFloat
    , className =? "viper-gui"                      --> doFloat
    , className =? "mpv"                            --> doFloat
    , className =? "xdman-Main"                     --> doFloat
    , className =? "feh"                            --> doFloat
    -- Xmessage is the error you get when you do something
    -- wrong in this config.
    , className =? "Xmessage"                       --> doFloat
    -- stops applications from spawning as master window
    , className =? "Termite"                        --> doF W.swapDown
    , className =? "URxvt"                          --> doF W.swapDown
    , className =? "Thunar"                         --> doF W.swapDown
    -- Assign to specific workspaces
    , className =? "Waterfox"                       --> doShift "w:\xe007"
    , className =? "Brave-browser"                  --> doShift "w:\xe007"
    , className =? "qutebrowser"                    --> doShift "w:\xe007"
    , className =? "Pcmanfm"                        --> doShift "e:\xf07b"
    , className =? "libreoffice-startcenter"        --> doShift "r:\xf044"
    , className =? "libreoffice-writer"             --> doShift "r:\xf044"
    , className =? "libreoffice-calc"               --> doShift "r:\xf044"
    , className =? "libreoffice-impress"            --> doShift "r:\xf044"
    , className =? "libreoffice-math"               --> doShift "r:\xf044"
    , className =? "viper-gui"                      --> doShift "t:\xf7d9"
    , className =? "Pavucontrol"                    --> doShift "t:\xf7d9"
    , className =? "Virt-manager"                   --> doShift "y:\xf108"
    , className =? "krita"                          --> doShift "a:\xf53f"
    , className =? "Gimp"                           --> doShift "a:\xf53f"
    , className =? "TelegramDesktop"                --> doShift "s:\xf3fe"
    --, className =? "name"                         --> doShift "6"
    --, className =? "name"                         --> doShift "6"
    --, className =? "name"                         --> doShift "7"

    -- i think, this means let applications do what the want
    , resource  =? "desktop_window"                 --> doIgnore
    , resource  =? "kdesktop"                       --> doIgnore
    ] <+> namedScratchpadManageHook myScratchPads

----------------------------------------------------------------------
-- SCRATCH PADS                                                     --
----------------------------------------------------------------------

-- ripped from distrotube's configs pretty much. it took me a while to
-- get my head around it, even after reading the docs.

-- i didn't realise that the title was something you set from the command
-- to launch the terminal.. despite it being written in right front of me.
-- use your terminal's man page to find the option for this.

-- you're on your own with the manage term bit. w & h refer to % of
-- screen. the other two, they are the distance from a fixed point.
-- where that fixed point is exactly... nobody knows.

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "term1" spawnTerm1 findTerm1 manageTerm1
                , NS "term2" spawnTerm2 findTerm2 manageTerm2
                , NS "term3" spawnTerm3 findTerm3 manageTerm3
                ]
    where
    spawnTerm1  = myTerminal ++ " -t term1"
    findTerm1   = title =? "term1"
    manageTerm1 = customFloating $ W.RationalRect l t w h
                where
                h = 0.4
                w = 0.75
                t = 0.40 -h
                l = 0.88 -w
    spawnTerm2  = myTerminal ++ " -t term2"
    findTerm2   = title =? "term2"
    manageTerm2 = customFloating $ W.RationalRect l t w h
                where
                h = 0.15
                w = 0.75
                t = 0.99 -h
                l = 0.88 -w

    spawnTerm3  = myTerminal ++ " -t term3"
    findTerm3   = title =? "term3"
    manageTerm3 = customFloating $ W.RationalRect l t w h
                where
                h = 0.5
                w = 0.80
                t = 0.50 -h
                l = 0.88 -w


 -- you can sink scratchpads into tiled layouts and still toggle them
 -- with their respective keys. i'd prefer it if toggling a scratchpad
 -- would spawn a new one after sinking the old one. maybe that's
 -- possible.

-----------------------------------------------------------------------
-- STARTUP HOOK
-----------------------------------------------------------------------

-- haven't had much luck with this trayer script, which supposedly
-- expands to accommodate icons. it just obscures the view of the other
-- widgets, so i think i disabled it and just put my widgets before
-- the tray.

myStartupHook = do
          spawn "trayer --edge top --align right --widthtype request --expand true --SetDockType true --SetPartialStrut true --transparent true --alpha 100 --tint 0x1A1918 --expand true --heighttype pixel --height 20 --monitor 1 --padding 1 &"
          spawn "viper --minimize &"
          spawn "nm-applet &"

-----------------------------------------------------------------------
-- MAIN
-----------------------------------------------------------------------

-- as i understand it, anything which differs from the default xmonad
-- config must be pipped through here. my mouse settings are the default
-- mouse settings, but if i were to change them, i'd add
-- ", mouseBindings = myMouseBindings," to this list, and provide my own
-- settings above.


main = do
        xmproc <- spawnPipe "xmobar ~/.config/xmobar/.xmobarrc"
        xmonad $ docks $ fullscreenSupport def
                --{ layoutHook = avoidStruts  $  layoutHook def
                { layoutHook = myLayout
                -- ripped from distrotube
                , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppCurrent = xmobarColor "#46a8c3" "" . wrap "[" "]" -- Current workspace in xmobar
                        , ppVisible = xmobarColor "#46a8c3" "" . wrap "" ""     -- Visible but not current workspace
                        , ppHidden = xmobarColor "#5294e2" "" . wrap "" ""   -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor "#2f343f" ""        -- Hidden workspaces (no windows)
                        , ppTitle = xmobarColor "gray" "" . shorten 60     -- Title of active window in xmobar
                        , ppSep =  "<fc=gray> | </fc>"                     -- Separators in xmobar
                        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        }
                , manageHook = myManageHook <+> fullscreenManageHook-- <+> manageHook defaultConfig
                , keys = myKeys
                , handleEventHook = fullscreenEventHook
                , startupHook = myStartupHook
                , workspaces = myWorkspaces
                , terminal           = myTerminal
                , normalBorderColor  = myNormalBorderColor
                , focusedBorderColor = myFocusedBorderColor
                , modMask            = mod4Mask
                , focusFollowsMouse  = myFocusFollowsMouse
                , clickJustFocuses   = myClickJustFocuses
                , borderWidth        = myBorderWidth
                }
