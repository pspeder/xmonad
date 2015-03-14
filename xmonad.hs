import XMonad
import Data.Ratio ((%))
import qualified XMonad.StackSet as W
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Maximize
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.WindowProperties (Property(..))
import XMonad.Actions.GridSelect
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.Search as S
import XMonad.Actions.Commands
import XMonad.Prompt
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Layout.WindowNavigation
import XMonad.Actions.TopicSpace
import XMonad.Actions.SinkAll
import XMonad.Actions.RotSlaves
import CustomFloatKeys
import PScratchpads
import PSP.Topics

main :: IO()
main = xmonad =<< myConfig

--myConfig :: XConfig (Choose Tall (Choose Mirror
myConfig = do
    d <- spawnPipe "dzen2 -p -ta l -e 'onstart=lower'"
    checkTopicConfig myTopics myTopicConfig
    return $ withUrgencyHook NoUrgencyHook $ ewmh $ defaultConfig
        { modMask           = mod4Mask
        , terminal          = "urxvtcd"
        , workspaces        = myTopics
        , startupHook       = do ewmhDesktopsStartup
                                 setWMName "LG3D"
                                 return ()
        , handleEventHook   = ewmhDesktopsEventHook
        , manageHook        = manageHook defaultConfig <+> manageDocks <+> pspNamedScratchpadManageHook <+> myManageHook [(ClassName "Gimp")] [] [] [] [] []
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
        ] ++ [ ("M-" ++ [n]) | n <- show [1..9] ])
        `additionalKeysP` (
        [ ("M-<Return>"             , spawnShell                                )
        , ("M-<Tab>"                , rotAllUp                                  )
        , ("M-S-<Tab>"              , rotAllDown                                )
        , ("M-w"                    , gridselectWorkspace defaultGSConfig W.view)
        --, ("M-q"                    , search                                    )
        , ("M-y"                    , commands >>= runCommand                   )
        , ("M-S-w"                  , spawn $ "notify-send " ++ show myNumberedTopics)
        , ("M-S-l"                  , sendMessage NextLayout                    )
        , ("M-f"                    , withFocused $ sendMessage.maximizeRestore ) -- make window full screen
        --, ("M-m"                    , windows W.focusMaster                     ) -- focus master window
        --, ("M-S-m"                  , windows W.swapMaster                      ) -- make current master window
        -- | shift windows focus
        , ("M-S-f h"                , sendMessage $ Go L                        ) -- shift focus left
        , ("M-S-f j"                , sendMessage $ Go D                        ) -- shift focus down
        , ("M-S-f k"                , sendMessage $ Go U                        ) -- shift focus up
        , ("M-S-f l"                , sendMessage $ Go R                        ) -- shift focus right
        -- | swap windows
        , ("M-S-s h"                , sendMessage $ Swap L                      ) -- swap window left
        , ("M-S-s j"                , sendMessage $ Swap D                      ) -- swap window downward
        , ("M-S-s k"                , sendMessage $ Swap U                      ) -- swap window upward
        , ("M-S-s l"                , sendMessage $ Swap R                      ) -- swap window right
        -- | resize floats -- make these their own function
        , ("M-r h"                  , withFocused (keysResizeWindow (-10,0) (1%2,1%2)))
        , ("M-r j"                  , withFocused (keysResizeWindow (0,-10) (1%2,1%2)))
        , ("M-r k"                  , withFocused (keysResizeWindow (0, 10) (1%2,1%2)))
        , ("M-r l"                  , withFocused (keysResizeWindow ( 10,0) (1%2,1%2)))
        -- | move floats   -- same note as above
        , ("M-S-r h"                , withFocused (keysMoveWindow (-10,0))      )
        , ("M-S-r j"                , withFocused (keysMoveWindow (0, 10))      )
        , ("M-S-r k"                , withFocused (keysMoveWindow (0,-10))      )
        , ("M-S-r l"                , withFocused (keysMoveWindow ( 10,0))      )
        -- | Screen Brightness
        , ("XF86MonBrightnessDown"  , spawn "xbacklight -dec 8%"                 )
        , ("XF86MonBrightnessUp"    , spawn "xbacklight -inc 8%"                 )
        -- | Multimedia
        , ("<XF86AudioPlay>"        , spawn "spotify-remote.sh playpause"       ) -- completely stop music
        , ("<XF86AudioStop>"        , spawn "spotify-remote stop"               ) -- completely stop music
        , ("<XF86AudioNext>"        , spawn "spotify-remote.sh next"            ) -- play next song
        , ("<XF86AudioPrev>"        , spawn "spotify-remote.sh previous"        ) -- play previous song
        , ("<XF86AudioMute>"        , spawn "amixer -c1 set PCM 0dB"            ) -- mute volume
        , ("<XF86AudioRaiseVolume>" , spawn "amixer -c 0 set PCM 2dB+"          ) -- raise volume by 2dB
        , ("<XF86AudioLowerVolume>" , spawn "amixer -c 0 set PCM 2dB-"          ) -- lower volume by 2dB
        -- Remote Control
        --  Maybe, these should only be availble if beast is, and perhaps also dependent on workspace
        , ("S-<XF86AudioPlay>"      , spawn "mpc -h 192.168.1.70 toggle"        ) -- toggle music playing
        , ("S-<XF86AudioStop>"      , spawn "mpc -h 192.168.1.70 stop"          ) -- completely stop music
        , ("S-<XF86AudioNext>"      , spawn "mpc -h 192.168.1.70 next"          ) -- play next song
        , ("S-<XF86AudioPrev>"      , spawn "mpc -h 192.168.1.70 previous"      ) -- play previous song
        , ("S-<XF86AudioRaiseVolume>",spawn "mpc -h 192.168.1.70 volume +5"     ) -- raise volume by 5%
        , ("S-<XF86AudioLowerVolume>",spawn "mpc -h 192.168.1.70 volume -5"     ) -- lower volume by 5%
        ] ++ pspNamedScratchpadKeys ++ myTopicKeys)

-- Loghook
myLogHook h = dynamicLogWithPP $ defaultPP
    -- display current workspace as darkgrey on light grey (opposite of 
    -- default colors)
    { ppCurrent         = dzenColor "#303030" "#909090" . pad 
    -- display other workspaces which contain windows as a brighter grey
    , ppHidden          = dzenColor "#909090" "" . pad 
    -- display other workspaces with no windows as a normal grey
    , ppHiddenNoWindows = dzenColor "#606060" "" . pad 
    -- display the current layout as a brighter grey
    , ppLayout          = dzenColor "#909090" "" . pad 
    -- if a window on a hidden workspace needs my attention, color it so
    , ppUrgent          = dzenColor "#ff0000" "#803333" . pad . dzenStrip
    -- shorten if it goes over 100 characters
    , ppTitle           = shorten 100
    -- no separator between workspaces
    , ppWsSep           = ""
    -- put a few spaces between each object
    , ppSep             = " "
    -- output to the handle we were given as an argument
    , ppOutput          = hPutStrLn h
    }

xpc :: XPConfig
xpc = defaultXPConfig { bgColor  = "black"
                      , fgColor  = "grey"
                      , promptBorderWidth = 0
                      , position = Bottom
                      , height   = 15
                      , historySize = 256
                      }

searchSite = S.promptSearchBrowser xpc "ff3"
search     = [("g", searchSite S.google)
             ,("h", searchSite S.hoogle)
             ,("a", searchSite S.amazon)
             ,("i", searchSite S.imdb)
             ,("y", searchSite S.youtube)]

commands :: X [(String, X ())]
commands = defaultCommands
