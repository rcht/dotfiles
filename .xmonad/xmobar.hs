Config { font = "xft:Hack:size=10:bold:antialias=true"
       , bgColor = "black"
       , fgColor = "grey"
       , position = Top
       , lowerOnStart = True
       , commands = [ Run Cpu [] 10
                    , Run Wireless "wlp0s20f3" ["-t","<essid>"] 10
                    , Run Memory [ "-t", "Mem: <usedratio>%" ] 10
                    , Run Battery ["-t", "Batt: <left>% <timeleft>"] 10
                    , Run UnsafeStdinReader
                    , Run Com "/home/rachit/.local/bin/systraypad" [] "traypad" 100
    		    , Run Date "%H:%M:%S %a %b %_d " "date" 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "<icon=/home/rachit/.xmonad/haskell.xpm/> <fc=#666666>|</fc> %UnsafeStdinReader% }{ Wlan: %wlp0s20f3wi% <fc=#ff0000> %cpu% </fc><fc=#ccff00> %battery% </fc><fc=#00ff00> %date%</fc> %traypad%"
       }
