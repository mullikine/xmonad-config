import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.Script

-- both of these methods work
-- EZConfig has a new section that lets me to chords
import XMonad.Util.EZConfig
-- This lets me extend my current config the same way
-- google: Graphics.X11.ExtraTypes.XF86
-- https://hackage.haskell.org/package/X11-1.8/docs/Graphics-X11-ExtraTypes-XF86.html
import Graphics.X11.ExtraTypes.XF86
---- figure out why after mapping function keys, they stop working.
--it might be because of xkb or xmodmap. I think it's a bug with xmonad
--, ((modm, xK_F1     ), spawn "bri down") -- doesn't work
--compile xmonad

import Data.Monoid
import System.Exit
import XMonad.Actions.CopyWindow
import XMonad.Actions.RotSlaves
import XMonad.Actions.GridSelect
import XMonad.Actions.FloatSnap
import XMonad.Actions.CycleWS
import XMonad.Hooks.FadeInactive
import XMonad.Layout.Spacing
-- gaps are for equal spacing between windows and around edge of
-- screen https://randomlinuxstuff.wordpress.com/2014/03/06/xmonad-make-gaps-around-the-edge-of-the-screen-equal-to-the-gaps-between-windows/
import XMonad.Layout.Gaps
-- equalspacing needs to be compiles externally, not worth doing
--import XMonad.Layout.EqualSpacing
--import XMonad.Layout.SimpleDecoration
import XMonad.Util.Dmenu
import XMonad.Actions.WindowBringer
import XMonad.Actions.UpdatePointer
import XMonad.Actions.SinkAll
import Control.Monad
import Control.Monad (liftM2)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Actions.GroupNavigation
import XMonad.Actions.DynamicWorkspaces
--import XMonad.Layout.PerWorkspace

-- this enables xm-sendcommand
import XMonad.Hooks.ServerMode
import XMonad.Actions.Commands
import XMonad.Actions.WindowGo

--import XMonad hiding (|||)
--import XMonad.Layout.LayoutCombinators
--import qualified XMonad.StackSet as W
--import Data.Monoid -- for All

quitWithWarning :: X ()
quitWithWarning = do
    let m = "confirm quit"
    s <- dmenu [m]
    when (m == s) (io exitSuccess)

myTerminal      = "/usr/bin/xterm"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth   = 0
myModMask       = mod4Mask
myNumlockMask   = mod2Mask
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#442244"

-- If the media key names do not appear in xev, it's probably because
-- keys have been bound to them here. Strange bug.

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_y, xK_u, xK_i] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++
    [
    --((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    --((modm .|. shiftMask, xK_Return), spawn  "win xterm")
    ((modm .|. shiftMask, xK_Return), spawn  "xt -b sh-xt")
    , ((modm,               xK_p     ), spawn "win rotate")
    , ((modm,               xK_p     ), spawn "win dmenu")
    , ((modm .|. shiftMask, xK_f     ), spawn "win ffdmenu")
    , ((modm,               xK_n     ), spawn "win nvim-tmux")
    , ((modm,               xK_u     ), spawn "win xterm-tmux -b")
    , ((modm .|. mod1Mask,  xK_u     ), spawn "win xterm-tmux")
    , ((modm .|. mod1Mask .|. shiftMask, xK_u ), spawn "win xterm-tmux -h")
    , ((modm .|. shiftMask, xK_u     ), spawn "win vt100-tmux")
    , ((modm,               xK_i     ), spawn "win xterm-inv")
    , ((modm .|. shiftMask, xK_i     ), spawn "win vt100-inv")
    , ((modm .|. shiftMask, xK_m     ), spawn "win mail")
    , ((modm,               xK_slash ), spawn "win browser")
    , ((modm .|. shiftMask, xK_g     ), spawn "win control-center")
    , ((modm .|. mod1Mask,  xK_g     ), spawn "killall import")
    , ((modm .|. shiftMask, xK_l     ), spawn "win calculator")
    , ((0, xF86XK_Calculator), spawn "win calculator")
    , ((modm .|. controlMask, xK_a     ), spawn "win mixer")
    , ((modm .|. shiftMask .|. controlMask, xK_Return     ), spawn "win edit-clipboard")
    , ((modm,               xK_e     ), spawn "win edit-xmonad-config")
    , ((modm .|. shiftMask, xK_r     ), spawn "win ruler")
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm .|. shiftMask, xK_h     ), spawn "win screenshot screen &")
    , ((modm .|. controlMask, xK_h   ), spawn "win screenshot root &")
    , ((modm .|. mod1Mask, xK_w     ), spawn "win capture-text screen &")
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. controlMask, xK_space ), spawn "op stop-lag")
    , ((modm .|. shiftMask, xK_q     ), spawn "op suspend")
    , ((modm              , xK_q     ), spawn "win recompile-xmonad")
    , ((modm              , xK_Q     ), spawn "win recompile-xmonad") -- turn off capslock
    , ((modm,               xK_backslash ), spawn "win lock")
    , ((modm, xK_m     ), spawn "win screenshot root")

    , ((modm,               xK_c     ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_grave   ), nextMatch History (return True))
    , ((modm,               xK_Tab),  toggleWS)
    , ((modm,               xK_x     ), windows W.focusDown)
    , ((modm,               xK_z     ), windows W.focusUp  )
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm,               xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((modm .|. shiftMask, xK_t     ), sinkAll)
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    , ((modm .|. shiftMask, xK_x     ), quitWithWarning)

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
    , ((modm, xK_v ), windows copyToAll)
    , ((modm, xK_bracketleft), rotAllUp)
    , ((modm, xK_bracketright), rotAllDown)

    -- fn f3
    , ((0, 0x1008ff4a), spawn "win inv")
    -- fn f4
    , ((0, 0x1008ff4b), spawn "win uninv")

    -- these must work but the keyboard itself must be having problems
    -- They are working now. I think "xkbset m; xkbset exp =mousekeys;"
    -- may have fixed it

    , ((0, xF86XK_MonBrightnessUp), spawn "op bri up")
    , ((0, xF86XK_MonBrightnessDown), spawn "op bri down")
    , ((0, 0x1008ff05), spawn "op backlight up")
    , ((0, 0x1008ff06), spawn "op backlight down")
    , ((0, xF86XK_AudioRaiseVolume), spawn "a up")
    , ((0, xF86XK_AudioLowerVolume), spawn "a down")
    --, ((shiftMask, 0x1008ff03), spawn "bri min")
    --, ((shiftMask, 0x1008ff02), spawn "bri max")
    , ((shiftMask, 0x1008ff06), spawn "op backlight off")
    , ((shiftMask, 0x1008ff05), spawn "op backlight max")
    , ((shiftMask, xF86XK_AudioRaiseVolume), spawn "a max")

    -- these are the same
    , ((shiftMask, xF86XK_AudioLowerVolume), spawn "a off")
    , ((0, xF86XK_AudioMute), spawn "a mute")
    ]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

--onWorkspace "4" full $
myLayout = tiled ||| full
  where
     --simple  = simpleDeco shrinkText defaultTheme (layoutHook defaultConfig)
     --tiled   = simpleDeco shrinkText myTheme $ spacing 5 $ Tall nmaster delta ratio
     -- gaps are for equal spacing between windows and around edge of
     -- screen https://randomlinuxstuff.wordpress.com/2014/03/06/xmonad-make-gaps-around-the-edge-of-the-screen-equal-to-the-gaps-between-windows/
     --tiled   = spacing 10 $ gaps [(U,10),(D,10),(L,10),(R,10)] $ Tall nmaster delta ratio -- good
     tiled   = spacing 4 $ gaps [(U,4),(D,4),(L,4),(R,4)] $ Tall nmaster delta ratio -- good
     -- smartspacing will go fullscreen with only 1 window
     --tiled   = smartSpacing 20 $ Tall nmaster delta ratio -- good
     --tiled   = spacing 15 $ Tall nmaster delta ratio
     --tiled   = spacing 13 $ Tall nmaster delta ratio -- good
     --tiled   = spacing 5 $ Tall nmaster delta ratio
     --tiled   = spacing 0 $ Tall nmaster delta ratio
     --tiled    = spacing 0 ( Full )
     --tiled   = spacing 2 $ Tall nmaster delta ratio
     full    = spacing 0 ( Full )
     nmaster = 1
     ratio   = 1/2
     delta   = 10/100

-- WM_CLASS is resource
-- WM_NAME is className
-- to find the class name use xprop then cool for the last WM_NAME value
myManageHook = composeAll
    [ className =? "Vlc"                        --> viewShift "4"
    , className =? "vlc"                        --> viewShift "4"
    , className =? "mpv"                        --> doShift "4"
    , className =? "Tor Browser"                        --> doShift "9"
    {- , className =? "Steam"                        --> viewShift "9" -}
    {- , resource =? "zenity"                        --> viewShift "9" -}
    , className =? "MPlayer"                    --> viewShift "4"
    , className =? "dosbox"                      --> unfloat <+> viewShift "6"
    , className =? "epsxe_x64"                      --> unfloat <+> viewShift "6"
    , resource =? "PencilMainWindow"            --> viewShift "6"
    , resource =? "mupen64plus"                 --> unfloat <+> viewShift "6"
    -- this is for raytracer. only runs under Xephyr and don't know why
    , className =? "Shane Mulligan"             --> doFloat
    , className =? "Gpick"                      --> doFloat
    , className =? "GParted"                    --> viewShift "3"
    , resource  =? "mednafen"                   --> unfloat <+> viewShift "6"
    , resource =? "Shane Mulligan"              --> doFloat
    , className =? "Google-chrome"              --> viewShift "2"
    , className =? "google-chrome"              --> viewShift "2"
    , className =? "Chrome"                     --> viewShift "2"
    , className =? "Firefox"                    --> doShift "2"
    , className =? "qutebrowser"                    --> doShift "2"
    , className =? "Navigator"                    --> doShift "2"
    --, className =? "Firefox"                    --> viewShift "2"
    , className =? "Zathura"                    --> viewShift "5"
    , className =? "hl2_linux"                  --> viewShift "6"
    , className =? "FTL "                       --> viewShift "6"
    , className =? "Skype"                      --> viewShift "8"
    , className =? "Plugin-container"           --> viewShift "4"
    -- ‘resource’ is the first element in WM_CLASS
    , resource =? "delphi32.exe"                --> doShift "8"
    , resource =? "RegexBuddy.exe"              --> viewShift "6"
    , resource =? "010Editor.exe"               --> viewShift "6"
    , resource =? "SmoothDraw4.exe"             --> doFloat
    , resource =? "Launcher.exe"                --> viewShift "6"
    , resource =? "Shareaza.exe"                --> viewShift "9"
    , resource =? "SC2.exe"                     --> viewShift "6"
    , resource =? "expleror.exe"                --> viewShift "8"
    , resource =? "idaq.exe"                    --> doShift "8"
    -- this makes chemdraw go crazy
    --, resource =? "ChemDraw.exe"                --> viewShift "9"
    -- treat wine apps individually now
    --, className =? "Wine"                       --> viewShift "8"
    , className =? "Steam"                      --> doShift "3"
    , className =? "TelegramDesktop"                      --> doShift "8"
    {- WM_CLASS(STRING) = "Telegram", "" -}
    , resource  =? "slack"                      --> doShift "3"
    , resource  =? "PCSX"                       --> unfloat <+> viewShift "6"
    , resource  =? "gtk-gnutella"               --> doShift "5"
    , title     =? "COMMANCHE - Wine desktop"   --> doFloat <+> viewShift "6"
    , title     =? "ONI - Wine desktop"         --> doShift "6"
    , className =? "Meld"                       --> viewShift "3"
    , title     =? "AOHD - Wine desktop"        --> doShift "6"
    , title     =? "AOC - Wine desktop"         --> doShift "6"
    , title     =? "AOE - Wine desktop"         --> doShift "6"
    , title     =? "sc2 - Wine desktop"         --> doShift "6"
    , title     =? "myst - Wine desktop"        --> doShift "6"
    , title     =? "outlaws - Wine desktop"     --> doShift "6"
    , title     =? "avernum6 - Wine desktop"    --> viewShift "6"
    , title     =? "Red Shift"                  --> unfloat
    , title     =? "Trigger Editor"             --> doFloat
    , title     =? "Error"                      --> doFloat
    , title     =? "About"                      --> doFloat
    , title     =? "Open"                       --> doFloat
    , title     =? "Audacity"                       --> viewShift "6"
    , title     =? "Save As"                    --> doFloat
    , title     =? "Select Units..."            --> doFloat
    , title     =? "Condition Editor"           --> doFloat
    , title     =? "Effect Editor"              --> doFloat
    , resource  =? "ts.exe"                     --> unfloat <+> viewShift "9"
    , resource  =? "ts-alpha.exe"               --> unfloat <+> viewShift "9"
    -- don't use this because it runs even if already handled e.g.
    -- COMMANCHE OR AOHD
    --, resource  =? "explorer.exe"               --> doShift "7"
    , resource  =? "Steam.exe"                  --> doShift "8"
    , resource  =? "Code"                       --> doShift "3"
    , resource  =? "CDisplayEx.exe"             --> unfloat <+> viewShift "5"
    , resource  =? "Battle.net.exe"             --> viewShift "8"
    , className =? "ePSXe - Enhanced PSX emulator" --> unfloat <+> doShift "6"
    , className =? "Eclipse"                    --> viewShift "9"
    , resource  =? "XMathematica"               --> doFloat <+> doShift "9"
    , resource  =? "display"               --> unfloat
    , className =? "VirtualBox"                 --> doShift "3"
    , className =? "VirtualBox Manager"                 --> doShift "3"
    --, className =? "MPlayer"                    --> doFloat
    -- when using gimp in single window mode, we want it to be tiled.
    -- actually, gimp goes insane when tiled
    , className =? "Gimp"                       --> doFloat <+> viewShift "7"
    , className =? "drracket"                   --> unfloat <+> viewShift "3"
    , className =? "DrRacket"                   --> unfloat <+> viewShift "3"
    , className =? "Denemo"                     --> doFloat
    --, resource  =? "feh"                        --> doFloat
    , resource  =? "xclock"                     --> doFloat
    , resource  =? "Xephyr"                     --> doFloat
    , resource  =? "screenruler"                --> doFloat
    , resource  =? "Kruler"                     --> doFloat
    , resource  =? "sun-awt-X11-XFramePeer"     --> doFloat
    , resource  =? "gnome-panel"                --> doIgnore
    , resource  =? "kicker"                     --> doIgnore
    , resource  =? "desktop_window"             --> doIgnore
    , resource  =? "kdesktop"                   --> doIgnore ]
    where
    viewShift = doF . liftM2 (.) W.greedyView W.shift
    unfloat = ask >>= doF . W.sink

--myEventHook = mempty
myEventHook = serverModeEventHook

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 1.00

myStartupHook = execScriptHook "startup"

main = xmonad =<< statusBar myBar myPP toggleStrutsKey defaults
myBar = "xmobar"
myPP = xmobarPP { ppCurrent = xmobarColor "#ff9999" ""
                , ppSep = " "
                , ppTitle   = xmobarColor "#ff6666"  "" . shorten 1000
                , ppOrder   = \(ws:_:t:_)   -> [ws,t] }
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

--handleEventHook DestroyWindowEvent {} = n <- (length . index) `fmap` withWindowSet guard (n == 1) $ sendMessage $ JumpToLayout "name of initial layout here" return (All True)

defaults = defaultConfig {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
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
        --logHook            = myLogHook,
--, logHook = myLogHook xmobars >> historyHook
--        logHook            = myLogHook xmobars >> historyHook,
        --logHook            = myLogHook >> historyHook,
        logHook            = historyHook,
        startupHook        = myStartupHook
    } `additionalKeysP`

    -- without unbuffer, 'e' will go into an infinite loop
    [
     ("M4-M1-1", spawn "unbuffer spx")
    ,("M4-M1-2", spawn "unbuffer pcx")
    ,("M4-M1-3", spawn "unbuffer ogx")
    ,("M4-S-]", spawn "x11 rotate-right")
    ,("M4-S-[", spawn "x11 rotate-left")
    --("<XF86MonBrightnessDown>", spawn "bri down")
    --, ("M-x w", spawn "bri down")
    --("S-<F1>", spawn "bri down")
    ]