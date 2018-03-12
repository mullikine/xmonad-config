-- Need to specify the full path to my overriding zsh or tab complete gets errors
--myTerminal      = "/home/shane/local/bin/xterm -ls -js +l /home/shane/local/bin/zsh"
--myTerminal      = "/usr/bin/xterm -ls -js +l /usr/bin/zsh"



    --, ((0, 0x1008ff02), spawn "op bri up")
    --, ((0, 0x1008ff03), spawn "op bri down")
    --, ((0, 0x1008FF11), spawn "a up")
    --, ((0, 0x1008FF13), spawn "a down")

    --, ((modm,               xK_i     ), spawn "bash -c \"set -x; set -m; xterm -ls -e \\\"resize &> /dev/null; sleep 0.1; TERM=xterm-256color tmux attach -t localhost\\\"& PIDX=\\$!; sleep 0.1; xprop -id \\$(/home/shane/local/bin/getwid.sh \\$PIDX) -f WM_CLASS 8s -set WM_CLASS 'compton-trans';\"")
    --, ((modm .|. shiftMask, xK_p     ), spawn "gmrun")
    --, ((modm .|. shiftMask, xK_e     ), spawn "gvim-xclip.sh")
    --, ((modm .|. shiftMask, xK_p     ), spawn "paste.sh")
    --, ((modm .|. shiftMask, xK_z     ), spawn "gvim-xclip-paste.sh")
    --, ((modm .|. shiftMask, xK_f     ), spawn "ff")

    --, ((modm .|. shiftMask, xK_g     ), spawn "gnome-control-center")
    --, ((modm .|. shiftMask, xK_g     ), spawn "wololo.sh")
    --, ((modm .|. shiftMask, xK_g     ), spawn "aok-goatse.sh")
    --, ((modm .|. shiftMask, xK_g     ), spawn "gnome-terminal -e \"attach-tmux.sh\"")
    --, ((modm .|. shiftMask, xK_g     ), spawn "wololo-auto.sh")
    --, ((modm .|. controlMask .|. shiftMask, xK_g     ), spawn "killall wololo-auto.sh;rmdir /tmp/wololo-lock")
    --, ((modm .|. controlMask .|. shiftMask, xK_g     ), spawn "killall wololo.sh")

    --, ((modm .|. shiftMask, xK_f     ), spawn "ffdmenu")

    --, ((modm,               xK_p     ), spawn "win dmenu")

    , ((modm .|. controlMask .|. shiftMask, xK_g     ), spawn "killall aok-goatse.sh")

    , ((modm .|. shiftMask, xK_space ), spawn "lagsc2.sh")

    --, ((modm,               xK_n     ), spawn "ff forget")


    --, ((modm,               xK_n     ), refresh)
    --, ((modm,               xK_m     ), windows W.focusMaster  )
    --, ((modm .|. shiftMask, xK_v      ), selectWorkspace defaultXPConfig)

    --, ((modm .|. shiftMask, xK_o     ), windows W.swapWithCurrent  )


    --, ((0, 0x1008FF12), spawn "a mute")



--myTheme = defaultTheme { activeColor         = "#000000"
--                       , inactiveColor       = "#000000"
--                       , activeBorderColor   = "#000000"
--                       , inactiveBorderColor = "#000000"
--                       , activeTextColor     = "#CEFFAC"
--                       , inactiveTextColor   = "#669966"
--                       , decoHeight          = 16
--                       , decoWidth           = 10000
--                       , fontName            = "-*-fixed-*-r-*-*-12-*-*-*-*-*-*-*"
--                       }
