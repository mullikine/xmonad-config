
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import XMonad.Hooks.DynamicLog
import Data.Monoid
import System.Exit
import XMonad.Actions.CopyWindow
import XMonad.Actions.GridSelect
import XMonad.Actions.FloatSnap
import XMonad.Actions.CycleWS
--import XMonad.Actions.Run
--import XMonad.Layout.NoBorders
import XMonad.Hooks.FadeInactive
import XMonad.Layout.Spacing
--import XMonad.Layout.TwoPane
import XMonad.Util.Dmenu
import XMonad.Actions.WindowBringer
import XMonad.Actions.UpdatePointer
--import XMonad.Actions.OnScreen
import Control.Monad

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


quitWithWarning :: X ()
quitWithWarning = do
    let m = "confirm quit"
    s <- dmenu [m]
    when (m == s) (io exitSuccess)

-- Xmobar
--import XMonad.Hooks.DynamicLog
--import XMonad.Hooks.ManageDocks
--import XMonad.Util.Run(spawnPipe)
--import XMonad.Util.EZConfig(additionalKeys)
--import System.IO
-- End Xmobar

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "/home/shane/local/bin/xterm -ls -js +l"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
--myFocusFollowsMouse = False
myFocusFollowsMouse = True

-- Width of the window border in pixels.
--
myBorderWidth   = 0
--myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask
--myModMask       = mod2Mask -- multihead

-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
myNumlockMask   = mod2Mask
--myNumlockMask   = mod3Mask -- multihead

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1 ","2 ","3 ","4 ","5 ","6 ","7 ","8 ","9 "]

-- Border colors for unfocused and focused windows, respectively.
--
--myNormalBorderColor  = "#222222"
--myFocusedBorderColor = "#bbbbbb"
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#442244"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- changed to mod-{y,u,i}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    -- changed to mod-shift-{y,u,i}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_y, xK_u, xK_i] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    -- this font doesn't work in xdmx it seems
    --, ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu -fn '-adobe-helvetica-*-r-*-*-18-*-*-*-*-*-*-*'` && eval \"exec $exe\"")
    --, ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu -fn '-*-fixed-*-r-*-*-18-*-*-*-*-*-*-*'` && eval \"exec $exe\"")
    , ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu -fn '-*-fixed-*-r-*-*-20-*-*-*-*-*-*-*'` && eval \"exec $exe\"")
    --, ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")

    -- launch dark xterm tmux localhost
    --, ((modm,               xK_u     ), spawn "xterm -ls -e \"(sleep 1;resize &> /dev/null;notify-send resize)& TERM=xterm-256color tmux attach -t localhost\"")
    , ((modm,               xK_u     ), spawn "xterm -ls -e \"resize &> /dev/null; sleep 0.1; TERM=xterm-256color tmux attach -t localhost\"")

    ---- launch bright xterm tmux localhost
    --, ((modm,               xK_i     ), spawn "xterm -ls -rv -e \"resize &> /dev/null; TERM=vt220 tmux attach -t localhost\"")
    -- launch bright xterm tmux localhost
    , ((modm,               xK_i     ), spawn "bash -c \"set -x; set -m; xterm -ls -e \\\"resize &> /dev/null; sleep 0.1; TERM=xterm-256color tmux attach -t localhost\\\"& PIDX=\\$!; sleep 0.1; xprop -id \\$(/home/shane/local/bin/getwid.sh \\$PIDX) -f WM_CLASS 8s -set WM_CLASS 'xtermi';\"")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    --, ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_c     ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), quitWithWarning)
    --, ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

-- WindowBringer
    , ((modm, xK_f),  gotoMenu)
  --, ((modm .|. shiftMask, xK_g),  gotoMenu)
    , ((modm .|. shiftMask, xK_b),  bringMenu)
-- End WindowBringer

-- GridSelect
    , ((modm              , xK_g     ), goToSelected defaultGSConfig)
-- End GridSelect

-- FloatSnap
    , ((modm,               xK_Left),  withFocused $ snapMove L Nothing)
    , ((modm,               xK_Right), withFocused $ snapMove R Nothing)
    , ((modm,               xK_Up),    withFocused $ snapMove U Nothing)
    , ((modm,               xK_Down),  withFocused $ snapMove D Nothing)
    , ((modm .|. shiftMask, xK_Left),  withFocused $ snapShrink R Nothing)
    , ((modm .|. shiftMask, xK_Right), withFocused $ snapGrow R Nothing)
    , ((modm .|. shiftMask, xK_Up),    withFocused $ snapShrink D Nothing)
    , ((modm .|. shiftMask, xK_Down),  withFocused $ snapGrow D Nothing)
-- End FloatSnap

-- CycleWS
    , ((modm,               xK_s),  nextWS)
    , ((modm,               xK_w),  prevWS)
    , ((modm .|. shiftMask, xK_s),  shiftToNext)
    , ((modm .|. shiftMask, xK_w),  shiftToPrev)
    , ((modm,               xK_d),  nextScreen)
    , ((modm,               xK_a),  prevScreen)
    , ((modm .|. shiftMask, xK_d),  shiftNextScreen)
    , ((modm .|. shiftMask, xK_a),  shiftPrevScreen)
    , ((modm,               xK_z),  toggleWS)
-- End CycleWS

-- CopyWindow
  , ((modm, xK_v ), windows copyToAll) -- @@ Make focused window always visible
-- End CopyWindow
    ]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

---- FloatSnap
--        , ((modm,               button1), (\w -> focus w >> mouseMoveWindow w >> snapMagicMove (Just 50) (Just 50) w))
--        , ((modm .|. shiftMask, button1), (\w -> focus w >> mouseMoveWindow w >> snapMagicResize [L,R,U,D] (Just 50) (Just 50) w))
--        , ((modm,               button3), (\w -> focus w >> mouseResizeWindow w >> snapMagicResize [R,D] (Just 50) (Just 50) w))
---- End FloatSnap

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = tiled ||| Full
--myLayout = tiled ||| Mirror tiled ||| Full
--myLayout = tiled ||| Full ||| TwoPane (3/100) (1/2)
--myLayout = spacing 1 $ tiled ||| Mirror tiled ||| Full
--myLayout = spacing 10 $ tiled ||| Full
--myLayout = spacing 20 $ tiled ||| Mirror tiled ||| Full
--myLayout = spacing 3 $ tiled ||| Mirror tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 10/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"                    --> doFloat
    , className =? "Gimp"                       --> doFloat
    , resource  =? "feh"                        --> doFloat
    --, resource  =? "sun-awt-X11-XFramePeer"     --> doFloat
    , resource  =? "xclock"                     --> doFloat
    , resource  =? "Xephyr"                     --> doFloat
    , resource  =? "screenruler"                --> doFloat
    , resource  =? "gnome-panel"                --> doIgnore
    , resource  =? "kicker"                     --> doIgnore
    , resource  =? "desktop_window"             --> doIgnore
    , resource  =? "kdesktop"                   --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
--myLogHook = return ()
myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
            >> updatePointer (Relative 0.95 0.95)
    where fadeAmount = 1.00
--    where fadeAmount = 0.7
--    where fadeAmount = 0.85 -- ideal
--    where fadeAmount = 0.85 -- ideal
--    updatePointer (Relative 0.5 0.5)
--            >> updatePointer Nearest
--            >> updatePointer (Relative 0.5 0.5)
--            >> updatePointer (Relative 1 1)

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = xmonad =<< statusBar myBar myPP toggleStrutsKey defaults

-- Command to launch the bar.
myBar = "xmobar"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
--CEFFAC -- light yellow
--429942 -- leaf green
myPP = xmobarPP { ppCurrent = xmobarColor "#EE0000" "" }
--myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)


-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        --numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }
