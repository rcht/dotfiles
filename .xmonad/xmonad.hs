import XMonad
import System.Directory
import System.IO (hPutStrLn)

import XMonad.Actions.CycleWS

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.WindowSwallowing

import XMonad.Layout.Gaps
import XMonad.Layout.Magnifier
import XMonad.Layout.Spacing
import XMonad.Layout.ShowWName
import XMonad.Layout.SimplestFloat

import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myTerminal      = "alacritty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth   = 1

myModMask       = mod4Mask

-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
myWorkspaces    = ["MAIN","DEV","CONF","MEET","VMC","BG","SYS","VIRT","MISC"] 

myNormalBorderColor  = "#222222"
myFocusedBorderColor = "#ff0000"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm , xK_t), spawn $ XMonad.terminal conf)

    -- launch rofi
    , ((modm,               xK_p     ), spawn "LC_CTYPE=en_US.utf8 rofi -show run")

    , ((modm .|. shiftMask ,   xK_b ), spawn "LC_CTYPE=en_US.utf8 rofi-bluetooth")

    -- launch rofi pdf menu
    , ((modm .|. shiftMask, xK_p     ), spawn "rofi-pdf")

    -- close focused window
    , ((modm , xK_q     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

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
    , ((modm .|. shiftMask,               xK_Return     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- firefox / current browser
    , ((modm              , xK_b     ), spawn "firefox &")

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io exitSuccess)

    -- Restart xmonad
    , ((modm .|. shiftMask , xK_h     ), spawn "xmonad --recompile; xmonad --restart")

    -- toggle gaps
    , ((modm .|. shiftMask , xK_g ), sendMessage ToggleGaps)

    -- xmonad-keys
    , ((modm , xK_a ), spawn "xmonad-keys-gui")

    -- save a screenshot to ~/Pictures/screenshots
    , ((modm, xK_c ), spawn "flameshot full -c -p $HOME/Pictures/screenshots/ &")

    -- flameshot
    , ((modm .|. shiftMask, xK_c), spawn "flameshot gui")

    -- needed because flameshot won't abort without libnotify
    , ((modm .|. shiftMask, xK_a), spawn "killall flameshot")

    -- increase window magnifier
    , ((modm .|. controlMask, xK_plus), sendMessage MagnifyMore)

    -- decrease window magnifier
    , ((modm .|. controlMask, xK_minus), sendMessage MagnifyLess)

    
    , ((modm .|. controlMask   , xK_o    ), sendMessage ToggleOff  )
    , ((modm .|. controlMask .|. shiftMask, xK_o    ), sendMessage ToggleOn   )
    , ((modm .|. controlMask              , xK_m    ), sendMessage Toggle     )

    -- shift current focused window to the next workspace
    , ((modm .|. shiftMask, xK_Right), shiftToNext )


    -- shift current focused window to the prev workspace
    , ((modm .|. shiftMask, xK_Left), shiftToPrev )

    -- galaxy buds client
    ,((modm , xK_g), spawn "~/.local/bin/GalaxyBudsClient_Linux_64bit_Portable.bin &")


    -- Lower volume
    , ((0, xK_F2), spawn "amixer -q sset Master 3%-")

    -- Raise volume
    , ((0, xK_F3), spawn "amixer -q sset Master 3%+")

    -- Toggle mute 
    , ((0, xK_F1), spawn "amixer -D pulse set Master 1+ toggle" )
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
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
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


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

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

mySWNConfig :: SWNConfig
mySWNConfig = def{
    swn_font = "xft:Fira Code:bold:size=50"
    , swn_fade = 0.25
    , swn_bgcolor = "#000000"
    , swn_color = "#ffffff"
}

myLayout =  magnifierOff $ showWName' mySWNConfig  $ avoidStruts myDefaultLayout
  where
    -- spacing between windows
    -- myDefaultSpacing = spacingRaw True (Border 0 2 2 2) True (Border 2 2 2 2) True

    -- Gap left on the side of windows 
    -- myDefaultGaps = gaps [(U, 20), (L, 0), (R, 0), (D, 0)] 

    --- Make a default layout so it's easy to add modifiers. 
    myDefaultLayout = tiled ||| Mirror tiled ||| Full ||| simplestFloat

    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

myEventHook = swallowEventHook (className =? "Alacritty") (return True)
-- myEventHook = mempty


myStartupHook = do
    spawnOnce "nitrogen --restore &"
    spawnOnce "picom &"
    spawnOnce "dunst &"
    -- spawnOnce "volumeicon &"
    -- spawnOnce "killall trayer; exec trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --height 21 --widthtype percent --width 4 --alpha 0 --tint 0x000000"
    -- spawnOnce "nm-applet &"
    spawnOnce "~/.local/bin/wifi-connect &"

main = do
    xmproc <- spawnPipe "LC_CTYPE=en_US.utf8 xmobar -x 0 $HOME/.xmonad/xmobar.hs"
    xmonad $ docks def{
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        -- numlockMask deprecated in 0.9.1
        -- numlockMask        = myNumlockMask,
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
        logHook            = dynamicLogWithPP $ xmobarPP {
            ppOutput = hPutStrLn xmproc,
            ppCurrent = xmobarColor "#ccff00" "" . wrap "[" "]",
            ppVisible = xmobarColor "#ccff00" "",
            ppHidden = xmobarColor "#ccff00" "",
            ppHiddenNoWindows = xmobarColor "#918db1" "",
            ppTitle = xmobarColor "#ffffff" "" . shorten 50,
            ppSep =  "  ",
            ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!",
            ppExtras  = [],
            ppOrder  = \(ws:l:t:ex) -> ws:[l]
        },
        startupHook        = myStartupHook
    }
