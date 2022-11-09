import System.Directory
import System.IO (hPutStrLn)

import XMonad

import XMonad.Actions.CycleWS
import XMonad.Actions.NoBorders

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
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

import qualified XMonad.StackSet    as W
import qualified Data.Map           as M
import qualified Data.Maybe         as DM




myTerminal           = "alacritty"

myFocusFollowsMouse  :: Bool
myFocusFollowsMouse  = True

myBorderWidth        = 2

myModMask            = mod4Mask

myWorkspaces         = ["MAIN","DEV","CONF","MEET","VMC","BG","SYS","VIRT","MISC"] 
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#888888"

toggleFullscreen :: X () 
toggleFullscreen = do
    sendMessage ToggleStruts
    withFocused toggleBorder
    sendMessage ToggleGaps

-- command to launch galaxy buds client, I didn't want to mess up the table structure with the long environment variable 
gbudsClientCmd = "DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1 GalaxyBudsClient_Linux_64bit_Portable.bin"


myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm                , xK_t     ), spawn $ XMonad.terminal conf                              )
    , ((modm                , xK_p     ), spawn "LC_CTYPE=en_US.utf8 rofi -show run"                )
    , ((modm .|. shiftMask  , xK_b     ), spawn "LC_CTYPE=en_US.utf8 rofi-bluetooth"                )
    , ((modm .|. shiftMask  , xK_p     ), spawn "~/.local/bin/rofi-pdf"                             )
    , ((modm                , xK_q     ), kill                                                      )
    , ((modm                , xK_space ), sendMessage NextLayout                                    )
    , ((modm .|. controlMask, xK_t     ), toggleFullscreen                                          )
    , ((modm .|. shiftMask  , xK_space ), setLayout $ XMonad.layoutHook conf                        )
    , ((modm                , xK_n     ), refresh                                                   )
    , ((modm                , xK_Tab   ), windows W.focusDown                                       )
    , ((modm                , xK_j     ), windows W.focusDown                                       )
    , ((modm                , xK_k     ), windows W.focusUp                                         )
    , ((modm                , xK_m     ), spawn "~/.local/bin/rofi-cmus"                            )
    , ((modm .|. shiftMask  , xK_m     ), spawn $ myTerminal ++ " -e cmus"                          )
    , ((modm                , xK_Return), windows W.swapMaster                                      )
    , ((modm .|. shiftMask  , xK_j     ), windows W.swapDown                                        )
    , ((modm .|. shiftMask  , xK_k     ), windows W.swapUp                                          )
    , ((modm                , xK_h     ), sendMessage Shrink                                        )
    , ((modm                , xK_l     ), sendMessage Expand                                        )
    , ((modm .|. shiftMask  , xK_Return), withFocused $ windows . W.sink                            )
    , ((modm                , xK_comma ), sendMessage (IncMasterN 1)                                )
    , ((modm                , xK_period), sendMessage (IncMasterN (-1))                             )
    , ((modm                , xK_b     ), spawn "firefox &"                                         )
    , ((modm .|. shiftMask  , xK_q     ), io exitSuccess                                            )
    , ((modm .|. shiftMask  , xK_h     ), spawn "xmonad --recompile; xmonad --restart"              )
    , ((modm .|. shiftMask  , xK_g     ), sendMessage ToggleGaps                                    )
    , ((modm                , xK_a     ), spawn "alacritty -e ~/.local/bin/xmonad-keys"             )
    , ((modm .|. shiftMask  , xK_c     ), spawn "flameshot gui"                                     )
    , ((modm                , xK_c     ), spawn "rofi -show calc -modi calc -no-show-match -no-sort")
    , ((modm .|. shiftMask  , xK_a     ), spawn "killall flameshot"                                 )
    , ((modm .|. controlMask, xK_plus  ), sendMessage MagnifyMore                                   )
    , ((modm .|. controlMask, xK_minus ), sendMessage MagnifyLess                                   )
    , ((modm .|. controlMask, xK_m     ), sendMessage Toggle                                        )
    , ((modm .|. shiftMask  , xK_Right ), shiftToNext                                               )
    , ((modm .|. shiftMask  , xK_Left  ), shiftToPrev                                               )
    , ((0                   , xK_F2    ), spawn "amixer -q sset Master 3%-"                         )
    , ((0                   , xK_F3    ), spawn "amixer -q sset Master 3%+"                         )
    , ((0                   , xK_F1    ), spawn "amixer -D pulse set Master 1+ toggle"              )
    , ((0                   , xK_F9    ), spawn "xfce4-terminal --drop-down"                        )
    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]




-- gestures = M.fromList
  -- [ ( [ L ], \_ -> nextWS )
  -- , ( [ U ], \_ -> nextWS )
  -- , ( [ R ], \_ -> prevWS )
  -- , ( [ D ], \_ -> prevWS )
  -- ]





myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    -- , ((0, button1), mouseGesture gestures)
    ]




mySWNConfig :: SWNConfig
mySWNConfig = def{
    swn_font      = "xft:Fira Code:bold:size=50"
    , swn_fade    = 0.25
    , swn_bgcolor = "#000000"
    , swn_color   = "#ffffff"
}




myLayout =  magnifierOff 
         $  showWName' mySWNConfig  
         -- $  spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True
         -- $  gaps [(U,30),(R,7),(L,7),(D,7)]
         $  avoidStruts myDefaultLayout
         where
            myDefaultLayout = tiled 
                          ||| Mirror tiled 
                          ||| Full 
                          ||| simplestFloat
            tiled   = Tall nmaster delta ratio
            nmaster = 1
            ratio   = 1/2
            delta   = 3/100




myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Xfce4-terminal" --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]




myEventHook = swallowEventHook (className =? "Alacritty") (return True)




myStartupHook = do
    spawnOnce "nitrogen --restore &"
    spawnOnce "picom &"
    spawn "~/.local/bin/wifi-connect &"






myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] 

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = DM.fromJust $ M.lookup ws myWorkspaceIndices





main = do
    xmproc <- spawnPipe "LC_CTYPE=en_US.utf8 xmobar -x 0 $HOME/.config/xmobar/xmobar.hs "
    xmonad $ ewmh $ docks $ def{
        terminal           = myTerminal          ,
        focusFollowsMouse  = myFocusFollowsMouse ,
        borderWidth        = myBorderWidth       ,
        modMask            = myModMask           ,
        workspaces         = myWorkspaces        ,
        normalBorderColor  = myNormalBorderColor ,
        focusedBorderColor = myFocusedBorderColor,

        keys               = myKeys              ,
        mouseBindings      = myMouseBindings     ,

        layoutHook         = myLayout            ,
        manageHook         = myManageHook        ,
        handleEventHook    = myEventHook         ,
        logHook            = dynamicLogWithPP $ xmobarPP {
            ppOutput          = hPutStrLn xmproc,
            ppCurrent         = xmobarColor "#fabd2f" "" . wrap "[" "]" . clickable,
            ppVisible         = xmobarColor "#fabd2f" ""                . clickable,
            ppHidden          = xmobarColor "#fabd2f" ""                . clickable,
            ppHiddenNoWindows = xmobarColor "#928374" ""                . clickable,
            ppTitle           = xmobarColor "#ffffff" "" . shorten 50              ,
            ppSep             =  "  "                                              ,
            ppUrgent          = xmobarColor "#C45500" "" . wrap "!" "!" . clickable,
            ppExtras          = []                                                 ,
            ppOrder           = \(ws:l:t:ex) -> ws:[l]
        },
        startupHook        = myStartupHook
    }
