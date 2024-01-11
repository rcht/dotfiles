Config { font = "Roboto Mono, Bold, 15"
       , bgColor = "#000000"
       , fgColor = "grey"
       , position = Top
       , lowerOnStart = True
       , commands = [ Run Cpu [] 10
                    , Run Uptime ["-t", "Up: <hours>h <minutes>m"] 10
                    , Run DynNetwork ["--", "--devices", "enp3s0f3u2,enp3s0f4u1,enp3s0f3u2,enp3s0f3u4"] 10
                    , Run Memory [ "-t", "Mem: <usedratio>%" ] 10
                    , Run Battery ["-t", "Batt: <left>% <timeleft>"] 10
                    , Run UnsafeStdinReader
                    -- , Run Com "/home/rachit/.local/bin/systraypad" [] "traypad" 100
    		    , Run Date "%H:%M:%S %a %b %_d " "date" 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "<icon=/home/rachit/.config/xmobar/haskell.xpm/>  %UnsafeStdinReader% }{ <fc=#fb4934> %uptime% </fc><fc=#fe8019> %dynnetwork% </fc><fc=#fabd2f> %cpu% </fc><fc=#b8bb26> %battery% </fc><fc=#83a598> %date% </fc>"
       }
