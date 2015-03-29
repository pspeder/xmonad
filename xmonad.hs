-- xmonad.hs
-- Main configuration file for Paw Saabye's XMonad setup.
import XMonad
import qualified XMonad.StackSet as W
import Control.Monad.Writer (All(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Util.Run (spawnPipe,hPutStrLn)
import XMonad.Actions.TopicSpace (checkTopicConfig)
import XMonad.Util.EZConfig (additionalKeysP,removeKeysP)
import XMonad.Util.WindowProperties (Property(..))
import XMonad.Hooks.ManageDocks (manageDocks)
import XMonad.Actions.GridSelect (defaultGSConfig) --Will be replaced by something in PSP.Configs
import XMonad.Layout.LayoutHints (hintsEventHook)
import XMonad.Layout.Maximize (maximizeRestore)
import XMonad.Actions.RotSlaves
import XMonad.Actions.Commands (runCommand)
import XMonad.Hooks.DynamicLog
import CustomUrgencyHook
import PScratchpads (pspNamedScratchpadManageHook)
import PSP.Keys
import PSP.Topics
import PSP.Utils (goToSelectedWS)

main :: IO()
main = do
    d <- spawnPipe "dzen2 -p -ta l -e 'onstart=lower'"
    checkTopicConfig myTopics myTopicConfig
    xmonad $ myConfig d

myConfig d =
    withUrgencyHookC LibNotifyUrgencyHook urgencyConfig {suppressWhen = OnScreen, remindWhen = Repeatedly 5 90} $
    defaultConfig
        { modMask           = mod4Mask
        , terminal          = "urxvtcd"
        , workspaces        = myTopics
        , startupHook       = do ewmhDesktopsStartup
                                 setWMName "LG3D"
                                 return ()
        , handleEventHook   = ewmhDesktopsEventHook <+> fullscreenEventHook <+>
                    (\e -> case e of
                        PropertyEvent{ ev_window = w } -> do
                            isURXVT <- runQuery (className =? "URxvt") w
                            if not isURXVT then hintsEventHook e else return (All True)
                        _ -> return (All True))
        , manageHook        = manageHook defaultConfig <+> manageDocks <+> pspNamedScratchpadManageHook <+>
                              myManageHook [(ClassName "Gimp")]     -- ts
                                           []                       -- fs
                                           [(ClassName "Skype" `And` Title "Options")] -- cfs
                                           []                       -- ffs
                                           []                       -- is
                                           []                       -- mss
        , layoutHook        = myTopicLayoutHook
        , logHook           = logHook defaultConfig >> ewmhDesktopsLogHook >> myLogHook d
        }
        `removeKeysP` (
        [ "M-<Space>"       -- Originally layout toggle
        , "M-S-<Space>"     --
        , "M-m"
        , "M-w"             -- First screen
        , "M-S-w"           -- Move to first screen
        , "M-e"             -- Second screen
        , "M-S-e"           -- Move to second screen
        , "M-r"             -- Third screen
        , "M-S-r"           -- Move to Third screen
        , "M-S-<Return>"    -- Spawn terminal
        , "M-<Return>"      -- Reset layout
        , "M-S-/"           -- Show keybinding
        , "M-q"             -- Restart XMonad
        , "M-z"
        ] ++ [ ("M-" ++ s ++ [n]) | n <- show [1..9]
                                  , s <- ["S-", ""] ])
        `additionalKeysP` (
        ("M-<Return>"             , spawnShell                                ):
        (myTopicKeys $
        [ ("M-<Tab>"                , rotAllUp                                  )
        , ("M-S-<Tab>"              , rotAllDown                                )
        , ("M-w"                    , goToSelectedWS myTopicConfig True defaultGSConfig)
        , ("M-y"                    , keysCommands >>= runCommand                   )
        , ("M-S-l"                  , sendMessage NextLayout                    )
        , ("M-f"                    , withFocused $ sendMessage.maximizeRestore ) -- make window full screen
        --, ("M-m"                    , windows W.focusMaster                     ) -- focus master window
        --, ("M-S-m"                  , windows W.swapMaster                      ) -- make current master window
        -- | Screen Brightness
        , ("XF86MonBrightnessDown"  , spawn "xbacklight -dec 8%"                )
        , ("XF86MonBrightnessUp"    , spawn "xbacklight -inc 8%"                )
        ]) ++ keysNamedScratchpads)

myLogHook h = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }

myDzenStatus = "dzen2 -w '320' -ta 'l'" ++ myDzenStyle
myDzenConky  = "conky -c ~/.xmonad/conkyrc | dzen2 -x '320' -w '704' -ta 'r'" ++ myDzenStyle
myDzenStyle  = " -h '20' -fg '#777777' -bg '#222222' -fn 'arial:bold:size=9'"

myDzenPP  = dzenPP
    { ppCurrent = dzenColor "#3399ff" "" . wrap " " " "
    , ppHidden  = dzenColor "#dddddd" "" . wrap " " " "
    , ppHiddenNoWindows = dzenColor "#777777" "" . wrap " " " "
    , ppUrgent  = dzenColor "#ff0000" "" . wrap " " " "
    , ppSep     = ""
    , ppLayout  = dzenColor "#aaaaaa" "" . wrap "^ca(1,xdotool key super+space)· " " ·^ca()"
    , ppTitle   = dzenColor "#ffffff" ""
                    . wrap "^ca(1,xdotool key super+k)^ca(2,xdotool key super+shift+c)"
                           "                          ^ca()^ca()" . shorten 20 . dzenEscape
    }

