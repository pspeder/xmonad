module XMonad.Config.Psp.Keys
( powerMenu
, keysGridLayout
, keysLocalMedia
, keysRemoteMedia
, keysControlFloats
, keysNamedScratchpads
, keysCommands
, addRemoveKeysP
) where

import           Data.Ratio                         ( (%) )                             -- for keysControlFloats
import qualified XMonad.StackSet as W               (focusMaster)

import           XMonad
import           XMonad.Util.EZConfig               ( additionalKeysP,removeKeysP )
import           XMonad.Actions.FloatKeys           ( keysResizeWindow,keysMoveWindow )
import           XMonad.Actions.Navigation2D        ( Direction2D(..) )
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.Search as S
import           XMonad.Actions.TopicSpace          ( currentTopicDir )
import           XMonad.Layout.WorkspaceDir         ( changeDir )

import           XMonad.Layout.WindowNavigation     ( Navigate(..) )
import           XMonad.Actions.Commands            ( defaultCommands )
import           XMonad.Layout.Maximize             ( maximizeRestore )
import           XMonad.Actions.RotSlaves
import           XMonad.Actions.Commands            ( runCommand )
import           XMonad.Prompt.Ssh
import           XMonad.Prompt.XMonad
import           XMonad.Prompt.AppendFile
import           XMonad.Prompt.RunOrRaise
import           XMonad.Prompt.Man                  ( Man(..),manPrompt )

import           XMonad.Config.Psp.Topics           ( myTopicKeys, wsGo, myTopicConfig )
import           XMonad.Config.Psp.Utils            ( goToSelectedWS,spawnIn,spawnSelected', spawnShellIn )
import           XMonad.Config.Psp.Configs
import           XMonad.Config.Psp.Scratchpads      ( myScratches,NamedScratchpads,namedScratchpadAction )

addRemoveKeysP conf = conf `removeKeysP` keysToRemove `additionalKeysP` keysToAdd

keysToRemove :: [String]
keysToRemove = [ "M-<Space>"       -- Originally layout toggle
               , "M-S-<Space>"     --
               , "M-m"
               , "M-w"             -- First screen
               , "M-S-w"           -- Move to first screen
               , "M-e"             -- Second screen
               , "M-S-e"           -- Move to second screen
               , "M-r"             -- Third screen
               , "M-S-r"           -- Move to Third screen
               , "M-p"             -- Run program with dmenu
               , "M-S-p"           -- Run program with gmrun
               , "M-S-<Return>"    -- Spawn terminal
               , "M-<Return>"      -- Reset layout
               , "M-S-/"           -- Show keybinding
               , "M-q"             -- Restart XMonad
               , "M-z"
               ] ++ [ ("M-" ++ s ++ [n]) | n <- show [1..9]
                                         , s <- ["S-", ""] ]

keysToAdd :: [(String, X())]
keysToAdd = myTopicKeys (
        [ ("M-<Return>"             , spawnShell                                )
        , ("M-p s"                  , sshPrompt myXPConfig                      )
        , ("M-p d"                  , changeDir myXPConfig                      )
        , ("M-p x"                  , xmonadPrompt myXPConfig                   )
        , ("M-p m"                  , manPrompt myXPConfig                      )
        , ("M-p q"                  , do spawnIn "/home/psp/notes/" "date >> quicknote.txt"
                                         appendFilePrompt myXPConfig "/home/psp/notes/quicknote.txt")
        , ("M-p n"                  , spawn "gvim $(date +%F-%T).md"            )
        , ("M2-m f"                 , windows W.focusMaster                     )
        , ("M2-m +"                 , sendMessage $ IncMasterN 1                )
        , ("M-r"                    , runOrRaisePrompt myXPConfig               )
        , ("M-S-r"                  , restart "xmonad" True                     )
        , ("M-<Tab>"                , rotAllUp                                  )
        , ("M-S-<Tab>"              , rotAllDown                                )
        , ("M-w"                    , wsGo True myGSConfig                      )
        , ("M-y"                    , keysCommands >>= runCommand               )
        , ("M-S-l"                  , sendMessage NextLayout                    )
        , ("M-f"                    , withFocused $ sendMessage.maximizeRestore ) -- make window full screen
        --, ("M-m"                    , windows W.focusMaster                     ) -- focus master window
        --, ("M-S-m"                  , windows W.swapMaster                      ) -- make current master window
        -- | Screen Brightness
        , ("XF86MonBrightnessDown"  , spawn "xbacklight -dec 8%"                )
        , ("XF86MonBrightnessUp"    , spawn "xbacklight -inc 8%"                )
        ] ++ keysNamedScratchpads ++ powerMenu)

{- Above two were originally
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
        , "M-p"             -- Run program with dmenu
        , "M-S-p"           -- Run program with gmrun
        , "M-S-<Return>"    -- Spawn terminal
        , "M-<Return>"      -- Reset layout
        , "M-S-/"           -- Show keybinding
        , "M-q"             -- Restart XMonad
        , "M-z"
        ] ++ [ ("M-" ++ s ++ [n]) | n <- show [1..9]
                                  , s <- ["S-", ""] ])
        `additionalKeysP` (myTopicKeys (
        [ ("M-<Return>"             , spawnShell                                )
        , ("M-p s"                  , sshPrompt myXPConfig                      )
        , ("M-p d"                  , changeDir myXPConfig                      )
        , ("M-p x"                  , xmonadPrompt myXPConfig                   )
        , ("M-p m"                  , manPrompt myXPConfig                      )
        , ("M-p n"                  , do name <- spawnIn "/home/psp/notes/" "date  quicknote.txt"
                                         appendFilePrompt myXPConfig "/home/psp/notes/quicknote.txt")
        , ("M-r"                    , runOrRaisePrompt myXPConfig               )
        , ("M-<Tab>"                , rotAllUp                                  )
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
        ] ++ keysNamedScratchpads ++ powerMenu))
-}


-- | As defined in documentation for the module X.A.GridSelect.
--   Spawn a shell in the dir that is bound to current topic.
spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

powerMenu :: [(String, X())]
powerMenu = [( "M-q", spawnSelected' menu )]
    where menu = [ ("Shutdown"       , "sudo systemctl poweroff"        )
                 , ("Reboot"         , "sudo systemctl reboot"          )
                 , ("Hibernate"      , "sudo systemctl hibernate"       )
                 , ("Hybrid Sleep"   , "sudo systemctl hybrid-sleep"    )
                 , ("Recompile XMonad","killall dzen2 && " ++
                                       "xmonad --recompile && xmonad --restart")
                 , ("Restart XMonad" , "xmonad --restart"               )
                 , ("Lock screen"    , "i3lock; xset dpms force suspend")
                 , ("Turn off screen", "xset dpms force suspend"        ) ] :: [(String, String)]


-- | Keys for when in a grid-like layout
keysGridLayout :: [(String, X())]
keysGridLayout =
        -- | shift windows focus
        [ ("M-S-f h", sendMessage $ Go L  ) -- shift focus left
        , ("M-S-f j", sendMessage $ Go D  ) -- shift focus down
        , ("M-S-f k", sendMessage $ Go U  ) -- shift focus up
        , ("M-S-f l", sendMessage $ Go R  ) -- shift focus right
        -- | swap windows
        , ("M-S-s h", sendMessage $ Swap L) -- swap window left
        , ("M-S-s j", sendMessage $ Swap D) -- swap window downward
        , ("M-S-s k", sendMessage $ Swap U) -- swap window upward
        , ("M-S-s l", sendMessage $ Swap R)]-- swap window right

keysControlFloats :: [(String, X())]
keysControlFloats =
        -- resize floats
        [ ("M-r h", withFocused (keysResizeWindow (-10,0) (1%2,1%2)))
        , ("M-r j", withFocused (keysResizeWindow (0,-10) (1%2,1%2)))
        , ("M-r k", withFocused (keysResizeWindow (0, 10) (1%2,1%2)))
        , ("M-r l", withFocused (keysResizeWindow ( 10,0) (1%2,1%2)))
        -- move floats
        , ("M-S-r h", withFocused (keysMoveWindow (-10,0))          )
        , ("M-S-r j", withFocused (keysMoveWindow (0, 10))          )
        , ("M-S-r k", withFocused (keysMoveWindow (0,-10))          )
        , ("M-S-r l", withFocused (keysMoveWindow ( 10,0))          )]

keysRemoteMedia :: [(String, X())]
keysRemoteMedia =
        -- Maybe, these should only be availble if beast is, and perhaps also dependent on workspace
        [ ("S-<XF86AudioPlay>"      , spawn "mpc -h 192.168.1.70 toggle"   ) -- toggle music playing
        , ("S-<XF86AudioStop>"      , spawn "mpc -h 192.168.1.70 stop"     ) -- completely stop music
        , ("S-<XF86AudioNext>"      , spawn "mpc -h 192.168.1.70 next"     ) -- play next song
        , ("S-<XF86AudioPrev>"      , spawn "mpc -h 192.168.1.70 previous" ) -- play previous song
        , ("S-<XF86AudioRaiseVolume>",spawn "mpc -h 192.168.1.70 volume +5") -- raise volume by 5%
        , ("S-<XF86AudioLowerVolume>",spawn "mpc -h 192.168.1.70 volume -5")]-- lower volume by 5%

keysLocalMedia :: [(String, X())]
keysLocalMedia =
        -- Multimedia
        [ ("<XF86AudioPlay>"       , spawn "spotify-remote.sh playpause") -- completely stop music
        , ("<XF86AudioStop>"       , spawn "spotify-remote stop"        ) -- completely stop music
        , ("<XF86AudioNext>"       , spawn "spotify-remote.sh next"     ) -- play next song
        , ("<XF86AudioPrev>"       , spawn "spotify-remote.sh previous" ) -- play previous song
        , ("<XF86AudioMute>"       , spawn "amixer -c1 set PCM 0dB"     ) -- mute volume
        , ("<XF86AudioRaiseVolume>", spawn "amixer -c 0 set PCM 2dB+"   ) -- raise volume by 2dB
        , ("<XF86AudioLowerVolume>", spawn "amixer -c 0 set PCM 2dB-"   )]-- lower volume by 2dB

{-
searchSite = promptSearchBrowser xpc "ff3"
search     = [("g", searchSite S.google)
             ,("h", searchSite S.hoogle)
             ,("a", searchSite S.amazon)
             ,("i", searchSite S.imdb)
             ,("y", searchSite S.youtube)]
-}

keysCommands = defaultCommands :: X [(String, X ())]

keysNamedScratchpads =
    [ ("M-<Space>", namedScratchpadAction myScratches "term"    )
    , ("M-s n",     namedScratchpadAction myScratches "notes"   )
    , ("M-s e",     namedScratchpadAction myScratches "editor"  )
    , ("M-s t",     namedScratchpadAction myScratches "tasks"   )
    , ("M-s i",     namedScratchpadAction myScratches "ghci"    )
    , ("M-s s",     namedScratchpadAction myScratches "mixer"   )
    , ("M-s h",     namedScratchpadAction myScratches "htop"    )
    , ("M-s p",     namedScratchpadAction myScratches "mail"    )
    , ("M-s m",     namedScratchpadAction myScratches "music"   )
    ]::[(String,    X())]

-- | Non-numeric num pad keys, sorted by number
keysNumericPad = [ xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down -- 1, 2, 3
                 , xK_KP_Left, xK_KP_Begin, xK_KP_Right     -- 4, 5, 6
                 , xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up   -- 7, 8, 9
                 , xK_KP_Insert]                            --    0
