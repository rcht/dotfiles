Config { font = "xft:Roboto Mono:size=11:bold:antialias=true"
       , bgColor = "black"
       , fgColor = "grey"
       , position = Top
       , lowerOnStart = True
       , commands = [ Run Cpu [] 10
                    , Run Uptime ["-t", "Up: <hours>h <minutes>m"] 10
                    , Run Wireless "wlp0s20f3" ["-t","Wlan: <essid>"] 10
                    , Run Memory [ "-t", "Mem: <usedratio>%" ] 10
                    , Run Battery ["-t", "Batt: <left>% <timeleft>"] 10
                    , Run UnsafeStdinReader
                    -- , Run Com "/home/rachit/.local/bin/systraypad" [] "traypad" 100
    		    , Run Date "%H:%M:%S %a %b %_d " "date" 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "<icon=/home/rachit/.xmonad/haskell.xpm/>  %UnsafeStdinReader% }{ <fc=#fb4934> %uptime% </fc><fc=#fe8019> %wlp0s20f3wi% </fc><fc=#fabd2f> %cpu% </fc><fc=#b8bb26> %battery% </fc><fc=#83a598> %date% </fc>"
       }
