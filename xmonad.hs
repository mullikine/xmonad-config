import XMonad
import XMonad.Hooks.DynamicLog
import Data.Monoid
import System.Exit
import XMonad.Actions.CopyWindow
import XMonad.Actions.RotSlaves
import XMonad.Actions.GridSelect
import XMonad.Actions.FloatSnap
import XMonad.Actions.CycleWS
import XMonad.Hooks.FadeInactive
import XMonad.Layout.Spacing
import XMonad.Layout.SimpleDecoration
import XMonad.Util.Dmenu
import XMonad.Actions.WindowBringer
import XMonad.Actions.UpdatePointer
import Control.Monad
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

quitWithWarning :: X ()
quitWithWarning = do
    let m = "confirm quit"
    s <- dmenu [m]
    when (m == s) (io exitSuccess)

myTerminal      = "/home/shane/local/bin/xterm -ls -js +l"
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myBorderWidth   = 0
myModMask       = mod4Mask
myNumlockMask   = mod2Mask
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#442244"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_y, xK_u, xK_i] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++

    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu -fn '-*-fixed-*-r-*-*-20-*-*-*-*-*-*-*'` && eval \"exec $exe\"")
    , ((modm,               xK_u     ), spawn "xterm -ls -e \"resize &> /dev/null; sleep 0.1; TERM=xterm-256color tmux attach -t localhost\"")
    , ((modm,               xK_i     ), spawn "bash -c \"set -x; set -m; xterm -ls -e \\\"resize &> /dev/null; sleep 0.1; TERM=xterm-256color tmux attach -t localhost\\\"& PIDX=\\$!; sleep 0.1; xprop -id \\$(/home/shane/local/bin/getwid.sh \\$PIDX) -f WM_CLASS 8s -set WM_CLASS 'xtermi';\"")
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm,               xK_c     ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_n     ), refresh)
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm,               xK_m     ), windows W.focusMaster  )
    , ((modm,               xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    , ((modm .|. shiftMask, xK_q     ), quitWithWarning)
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    , ((modm, xK_f),  gotoMenu)
    , ((modm .|. shiftMask, xK_b),  bringMenu)
    , ((modm              , xK_g     ), goToSelected defaultGSConfig)
    , ((modm,               xK_Left),  withFocused $ snapMove L Nothing)
    , ((modm,               xK_Right), withFocused $ snapMove R Nothing)
    , ((modm,               xK_Up),    withFocused $ snapMove U Nothing)
    , ((modm,               xK_Down),  withFocused $ snapMove D Nothing)
    , ((modm .|. shiftMask, xK_Left),  withFocused $ snapShrink R Nothing)
    , ((modm .|. shiftMask, xK_Right), withFocused $ snapGrow R Nothing)
    , ((modm .|. shiftMask, xK_Up),    withFocused $ snapShrink D Nothing)
    , ((modm .|. shiftMask, xK_Down),  withFocused $ snapGrow D Nothing)
    , ((modm,               xK_s),  nextWS)
    , ((modm,               xK_w),  prevWS)
    , ((modm .|. shiftMask, xK_s),  shiftToNext)
    , ((modm .|. shiftMask, xK_w),  shiftToPrev)
    , ((modm,               xK_d),  nextScreen)
    , ((modm,               xK_a),  prevScreen)
    , ((modm .|. shiftMask, xK_d),  shiftNextScreen)
    , ((modm .|. shiftMask, xK_a),  shiftPrevScreen)
    , ((modm,               xK_z),  toggleWS)
  , ((modm, xK_v ), windows copyToAll)
  , ((modm .|. controlMask, xK_u), rotAllUp)
  , ((modm .|. controlMask, xK_i), rotAllDown)
    ]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

myLayout = tiled ||| full
  where
     --simple  = simpleDeco shrinkText defaultTheme (layoutHook defaultConfig)
     tiled   = simpleDeco shrinkText myTheme $ spacing 5 $ Tall nmaster delta ratio
     full    = spacing 0 ( Full )
     nmaster = 1
     ratio   = 1/2
     delta   = 10/100

myManageHook = composeAll
    [ className =? "MPlayer"                    --> doFloat
    , className =? "Gimp"                       --> doFloat
    , resource  =? "feh"                        --> doFloat
    , resource  =? "xclock"                     --> doFloat
    , resource  =? "Xephyr"                     --> doFloat
    , resource  =? "screenruler"                --> doFloat
    , resource  =? "sun-awt-X11-XFramePeer"     --> doFloat
    , resource  =? "gnome-panel"                --> doIgnore
    , resource  =? "kicker"                     --> doIgnore
    , resource  =? "desktop_window"             --> doIgnore
    , resource  =? "kdesktop"                   --> doIgnore ]

myEventHook = mempty

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
            >> updatePointer (Relative 0.95 0.95)
    where fadeAmount = 1.00

myStartupHook = return ()

main = xmonad =<< statusBar myBar myPP toggleStrutsKey defaults
myBar = "xmobar"
myPP = xmobarPP { ppCurrent = xmobarColor "#CEFFAC" ""
                , ppSep = " "
                , ppTitle   = xmobarColor "#CEFFAC"  "" . shorten 60
                , ppOrder   = \(ws:_:t:_)   -> [ws,t] }
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

defaults = defaultConfig {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

myTheme = defaultTheme { activeColor         = "#000000"
                       , inactiveColor       = "#000000"
                       , activeBorderColor   = "#000000"
                       , inactiveBorderColor = "#000000"
                       , activeTextColor     = "#CEFFAC"
                       , inactiveTextColor   = "#669966"
                       , decoHeight          = 24
                       , decoWidth           = 10000
                       , fontName            = "-*-fixed-*-r-*-*-18-*-*-*-*-*-*-*"
                       }
