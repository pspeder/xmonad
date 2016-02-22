module XMonad.Config.Psp (pspConfig) where

-- See the usage of imported functions for further details/implementation ideas

-- IMPORT SECTION:
--    (qualified) module.name (as shortname)        (imported function(s))  -- module/function description

-- ---------------------------------------
-- Utilities/Helpers from std Haskell libs
-- ---------------------------------------
import System.Exit
import Graphics.X11.Xlib.Cursor                     ( xC_left_ptr )
import Graphics.X11.Types                           ( mod4Mask )
import Data.Monoid                                  ( mconcat )
-- --------------------------
-- Imports specific to XMonad
-- --------------------------
import           XMonad                             ( defaultConfig         -- The config on which this is based
                                                    , catchIO
                                                    , (<+>)
                                                    , XConfig(..)
                                                    , spawn
                                                    , X(..) )
import           XMonad.Config.Kde                  ( kde4Config )          -- For qubes
import           XMonad.Util.WindowProperties       ( Property(..) )        -- Classes to handle window properties
import           XMonad.Util.Cursor                 ( setDefaultCursor )    -- Does what function name suggests
import           XMonad.Hooks.SetWMName             ( setWMName )           -- From documentation: "May be useful for making Java GUI programs work..."
import           XMonad.Hooks.EwmhDesktops          ( ewmh                  -- Make XMonad compliant with ewmh desktop specification
                                                    , ewmhDesktopsEventHook
                                                    , ewmhDesktopsLogHook
                                                    , ewmhDesktopsStartup )
import           XMonad.Hooks.CurrentWorkspaceOnTop ( currentWorkspaceOnTop )
import           XMonad.Hooks.ManageDocks           ( manageDocks )         -- Regard dock-type applications' properties
import           XMonad.Hooks.DynamicHooks          ( dynamicMasterHook )   -- Add support for dynamic ManageHooks

-- This ought to be in Hooks if you ask me
import           XMonad.Actions.UpdateFocus         ( adjustEventInput      -- Adjusts event mask to pick up pointer movements
                                                    , focusOnMouseMove )    -- Event hook handle for updating focus on mouse move
import           XMonad.Hooks.InsertPosition        ( insertPosition        -- Determine where on the stack new windows should be inserted
                                                    , Focus   (..)
                                                    , Position(..) )

import qualified XMonad.Actions.TopicSpace as TS    ( defaultTopicConfig, TopicConfig(..)
                                                    , currentTopicDir )
import           XMonad.Actions.TopicSpace          ( (>*>) )
import           XMonad.Actions.RotSlaves           ( rotSlavesUp,rotSlavesDown )

import           XMonad.Hooks.UrgencyHook           ( withUrgencyHookC, withUrgencyHook
                                                    , UrgencyConfig(..), urgencyConfig
                                                    , SuppressWhen(..), RemindWhen(..)
                                                    , NoUrgencyHook(..) )
import           XMonad.Layout.Fullscreen           ( fullscreenEventHook, fullscreenManageHookWith )

-- ----------------------
-- Non-std XMonad modules ( check ~/.xmonad/lib/<Module.Name.hs> for definitions)
-- ----------------------
import           XMonad.Hooks.LibNotifyUrgency      ( LibNotifyUrgencyHook(..) )
import           XMonad.Hooks.EventHooks            ( urxvtEventHook )
-- ----------------------------------------
-- Other config. files related to pspConfig
-- ----------------------------------------
import           XMonad.Config.Psp.Scratchpads      ( namedScratchpadAction, myScratches
                                                    , pspNamedScratchpadManageHook )
import           XMonad.Config.Psp.Topics           ( myTopics, myTopicConfig )
import           XMonad.Config.Psp.Keys
import           XMonad.Config.Psp.LogHook
--import           XMonad.Config.Psp.Configs          ( pspProgs )
import           XMonad.Config.Psp.Layouts          ( myTopicLayoutHook, myStandardLayout )
import           XMonad.Actions.TopicDefinitions  --( TopicDefinition(..),TopicDefinition, topicStartupHook )

spawn' :: String -> X()
spawn' app = TS.currentTopicDir myTopicConfig >>= (spawner app)
    where spawner :: String -> String -> X()
          spawner app' dir = spawn $ "cd '" ++ dir ++ "' && '" ++ app' ++ "'"

-- | As defined in documentation for the module X.A.GridSelect.
--   Spawn a shell in the dir that is bound to current topic.
spawnShell :: X ()
spawnShell = TS.currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: String -> X ()
spawnShellIn dir = spawnIn "/bin/zsh" dir

-- | Spawn app from certain dir (gvim)
spawnIn :: String -> String -> X()
spawnIn app dir = spawn $ "cd " ++ dir ++ " && konsole -e " ++ app

pspConfig =
    withUrgencyHookC LibNotifyUrgencyHook
                     urgencyConfig { suppressWhen = OnScreen
                                   , remindWhen   = Repeatedly 5 90 }
  . withUrgencyHook  NoUrgencyHook
  . ewmh
  . addRemoveKeysP
  $ kde4Config
    { modMask           = mod4Mask
    , terminal          = "konsole"
    , workspaces        = topics pspTopicDefs
    , startupHook       = ewmhDesktopsStartup           <+>
                          setDefaultCursor xC_left_ptr  <+>
                          setWMName "LG3D"              <+>
                          adjustEventInput              <+>
                          topicStartupHook pspTDConfig pspTopicDefs pspProgs
    , handleEventHook   = ewmhDesktopsEventHook         <+>
                          focusOnMouseMove              <+>
                          fullscreenEventHook           <+>
                          urxvtEventHook
    , manageHook        = manageHook defaultConfig      <+>
                          dynamicMasterHook             <+>
                          insertPosition Below Newer    <+>
                          manageDocks                   <+>
                          pspNamedScratchpadManageHook  <+>
                          topicManageHook pspTopicDefs
                                          [(ClassName "Qubes-manager")]     -- ts
                                          []                       -- fs
                                          [(ClassName "Skype" `And`
                                            Title "Options")]      -- cfs
                                          []                       -- ffs
                                          []                       -- is
                                          []                       -- mss
    , layoutHook        = myTopicLayoutHook
    --, logHook           = logHook defaultConfig >>
    --                      currentWorkspaceOnTop >>
    --                      ewmhDesktopsLogHook   >>
    --                      pspLogHook d
    }

pspTDConfig = TDConfig
    { tdcDefaultTopic = head $ topics pspTopicDefs
    , tdcDefaultDir   = "~"
    , tdcDefultAction = const $ return ()
    , tdcMaxHistory   = 10
    , tdcMenuKey      = EZKey "M-m"
    , tdcStartupApps  = []
    , tdcProgSetup    = pspProgs
    }

pspProgs = ProgSetup
    { taHomeDir     = "/home/psp"
    , taDefaultDir  = "/home/psp"
    , taTerminal    = "konsole"
    , taPDF         = "mupdf"
    , taMail        = "mutt"
    , taMailCalendar= "thunderbird"
    , taEditor      = "gvim"
    , taBrowser     = "firefox "
    , taBrowserTab  = "firefox -new-tab "
    , taBrowserWin  = "firefox -new-window "
    , taPBrowserWin = "firefox -private-window "
    , taFloats      = [ Resource "xclock" ]
    , taCFloats     = [ ClassName "SMPlayer", ClassName "MPlayer", ClassName"XMessage"
                      , ClassName "XFontSel", ClassName "bashrun", ClassName "zshrun"
                      , Title "Google Chrome Options", Title "Chromium Options"]
    , taFullFloats  = [ ClassName "XBMC", ClassName "Kodi"]
    , taIgnores     = [ Resource "desktop",Resource "desktop_window",Resource "notify_osd", ClassName "Dunst"
                      , Resource "staleontray", Resource "trayer",Resource "dzen2", Resource "dzen2-bar"]
    , taStartup     = []--[spawn "pidgin"], safeSpawn "apulse32" ["skype"]]
    }

pspTopicDefs :: TopicDefinitions
pspTopicDefs =
  [ -- This holds information about the Topic/workspace 1:main
    -- Initialise a new TopicDefinition record from scratch - you must then
    -- specify each record field with desired value(s).
    defaultTopicDefinition
        { tdName            = "QVMM"
        , tdBoundApps       = [ClassName "Qubes-manager"]
        , tdMenuApps        =
            [ ("Terminal", taTerminal pspProgs)
            --, ("Run in", spawnInVM name )
            ]
        }
  , TopicDefinition
        { tdName            = "1:main"  -- @XMonad.StackSet.Workspace@'s name
        , tdAction          = -- X () action to spawn when workspace is chosen
                              spawnShell >*> 2
        , tdActionOnStartup = True      -- Should action be run on XMonad start?
        , tdActionOnFocus   = False     -- Should it be run when ws is selected?
        , tdHidden          = False     -- Should the workspace be hidden
        , tdDir             = "/home/psp"-- X.A.TopicSpace.Dir in which, for
                                        -- example, terminals are spawned
                                        -- (a lot of urxvt dependent stuff)
        , tdBoundApps       = []        -- X.U.WindowProperties.Property of apps
                                        -- that should be switched to this
                                        -- workspace when spawned with one of
                                        -- the utility functions of
                                        -- X.A.TopicDefinitions.
        , tdMenuApps        =           -- Apps that can be launched with a
                                        -- X.A.GridSelect menu via a shortcut
                                        -- set in TopicDefinitionSetup.
            [ ("Terminal"  , taTerminal pspProgs)
            , ("PDF Viewer", taPDF pspProgs     )
            , ("Editor"    , taEditor pspProgs  ) ]
        , tdKeyBindings     =           -- X.A.EZConfig key bindings that are
                                        -- bound to workspace with
                                        -- X.A.PerWorkspaceKeys.bindOn and
                                        -- bound to your XConfig with the
                                        -- utility function topicAddRemoveKeys
                                        -- of X.A.TopicDefinitions.
            [ ("M-z", spawnShell ) ]
        -----------------------------------------------------------------------
        -- If possible, future releases should also include a layout hook:
        --
        -- @, tdBoundLayout     = myStandardLayout@
        --
        -- ... Can't get types to match properly here, though.
        --
        -- See the layout hook further down or compose one yourself (possibly
        -- using some of the example layouts from XMonad.Config.PSP.Layouts).
        -----------------------------------------------------------------------
        }
  -- This is the second workspace 2:misc - for random stuff you just need to put
  -- away, or just for some temporary real estate. In my case, mostly for
  -- testing layouts and such.
  -- Defining the TopicDefinition by overriding some of the defaults from
  -- X.A.TopicDefinitions(defaultTopicDefinition)
  , defaultTopicDefinition
        { tdName            = "2:misc"
        , tdMenuApps        = []
        }
  -- Or you could initialise it specifying the record fields to the constructor.
  -- They must be supplied in the following order (the other definitions here do
  -- too) and all arguments must be supplied.
  -- Less typing - more difficult to get right.
  --                tdName        tdDir            tdHidden
  , TopicDefinition "3:organiser" "/home/psp/mail" False
  --    tdAction                  tdActionOnStartup   tdActionOnFocus
        (spawn $ taMail pspProgs) True                False
  --    tdBoundApps
        [ ClassName "Thunderbird", Resource "mutt" ]
  --    tdMenuApps
        [ ("Mail and Calendar", taMailCalendar pspProgs     )
        , ("Mail (mutt)"      , taMail pspProgs             )
        , ("Notes"            , "urxvt -cd '/home/psp/notes' -e vim -c ':e .'") ]
  --    tdKeyBindings
        []
  -- ... make n of one of these, where n is the number workspaces you want.
  -- it is important that tdName is set uniquely for each TopicDefinition.
  , defaultTopicDefinition
        { tdName            = "4:dev"
        , tdDir             = "/home/psp/dev"
        , tdAction          = spawn' "gvim" >>
                              spawnIn "/home/psp/dev" "urxvt -role 'editorterm"
        , tdActionOnStartup = True
        , tdBoundApps       =
            [ ClassName "Gvim"
            , ClassName "Netbeans IDE 8.0.2"
            , ClassName "Eclipse"
            , ClassName "Brackets"
            , ClassName "Tea"
            , ClassName "Gedit"
            , ClassName "sublime_text"
            , ClassName "Notepadqq"
            , ClassName "jetbrains-idea"
            , ClassName "jetbrains-android-studio"
            , ClassName "Mousepad"
            , Role "Editor" ]
        , tdMenuApps        =
            [ ("GVIM"          , "gvim --role 'dev-gvim'"           )
            , ("Netbeans"      , "netbeans"                         )
            , ("Eclipse"       , "eclipse"                          )
            , ("Sublime Text 2", "subl"                             )
            , ("Notepadqq"     , "notepadqq"                        )
            , ("IntelliJ"      , "intellij-idea-ultimate-edition"   )
            , ("PDF Viewer"    , taPDF pspProgs                       )
            , ("PDF Viewer"    , "mupdf"                            ) ]
        , tdKeyBindings     =
            [ ("M-<Tab>"       , rotSlavesDown   )
            , ("M-S-<Tab>"     , rotSlavesUp     )
            -- won't need to push stuff into tiling on this ws
            , ("M-t"           , namedScratchpadAction myScratches "editorterm") ]
        }
  , defaultTopicDefinition
        { tdName            = "5:www"
        , tdDir             = "/home/psp/downloads"
        , tdAction          = spawn $ taBrowser pspProgs
        , tdBoundApps       =
            [ ClassName "Firefox"
            , ClassName "Midori"
            , ClassName "Google-chrome-stable"
            , ClassName "Chromium"
            , ClassName "Opera"
            , ClassName "Vimprobable"
            , ClassName "Xombrero"
            , Role "browser" ]
        , tdMenuApps        =
            [ ("Midori"     , "midori"        )
            , ("Firefox"    , "firefox"       )
            , ("Chrome"     , "chrome"        )
            , ("Chromium"   , "chromium"      )
            , ("Opera"      , "opera"         )
            , ("Vimprobable", "vimprobable"   )
            , ("Xombrero"   , "xombrero"      ) ]
        , tdKeyBindings     =
            [ ("M-b w", spawn' $ taBrowserWin pspProgs   )
            , ("M-b t", spawn' $ taBrowserTab pspProgs   )
            , ("M-b p", spawn' $ taPBrowserWin pspProgs  ) ]
        }
  , defaultTopicDefinition
        { tdName            = "6:chat"
        , tdDir             = "/home/psp/downloads"
        , tdAction          = spawn' "pidgin" >> spawn "apulse32 skype"
        , tdActionOnStartup = True
        , tdActionOnFocus   = False
        , tdBoundApps       =
            [ ClassName "Skype"
            , ClassName "Pidgin"
            , ClassName "Xchat" ]
        , tdMenuApps        =
            [ ("Skype" , "skype" )
            , ("Pidgin", "pidgin") ]
        }
  , defaultTopicDefinition
        { tdName            = "7:vms"
        , tdDir             = "/home/psp/vms"
        , tdAction          = spawn' "virtualbox"
        , tdActionOnFocus   = True
        , tdBoundApps       =
            [ ClassName "VirtualBox" ]
        , tdMenuApps        =
            [ ("VirtualBox", "virtualbox"                  )
            , ("Win7 VM"   , "virtualbox --startvm win7"   )
            , ("fedbox VM" , "virtualbox --startvm fedbox" ) ]
        }
  , defaultTopicDefinition
        { tdName            = "8:media"
        , tdDir             = "/home/psp/images"
        , tdAction          = spawn' "gimp"
        , tdBoundApps       =
            [ ClassName "Gimp"
            , ClassName "Lives"
            , ClassName "Pitivi" ]
        , tdMenuApps        =
            [ ("The Gimp", "gimp"  )
            , ("Pitivi"  , "pitivi")
            , ("LiVES"   , "lives" ) ]
        }
  , defaultTopicDefinition
        { tdName            = "server"
        , tdDir             = "/home/psp/shares"
        , tdHidden          = True
        , tdAction          = spawnShell >> spawn "beastmouse"
        , tdActionOnFocus   = False
        , tdKeyBindings     =
            [ ("M-S-m p", spawn "ssh beast 'mpc toggle'")
            , ("M-S-m s", spawn "ssh beast 'mpc stop'"  )
            , ("M-S-m f", spawn "ssh beast 'mpc next'"  )
            , ("M-S-m b", spawn "ssh beast 'mpc prev'"  ) ]
        }
  , defaultTopicDefinition
        { tdName            = "remotes"
        , tdDir             = "/home/psp/shares"
        , tdHidden          = True
        , tdAction          = spawnShell >*> 5 -- possibly terminals to five most used connections
        , tdActionOnFocus   = False
        , tdBoundApps       =
            [ Resource "xfreerdp"
            , Resource "qsynergy"
            , ClassName "Teamviewer.exe" `And` (Not $ Resource "Actual window") ]
        , tdMenuApps        =
            [ ("XFreeRDP"   , "xfreerdp"    )
            , ("QSynergy"   , "qsynergy"    )
            , ("Teamviewer" , "teamviewer"  ) ]
        }
  , defaultTopicDefinition
        { tdName            = "configs"
        , tdDir             = "/home/psp/dev/Configs"
        , tdHidden          = True
        , tdActionOnFocus   = False
        , tdBoundApps       =
            [ (ClassName "Gvim" `And` Role "configEditor") ]
        , tdMenuApps        =
            [ ("Terminal"       , taTerminal pspProgs)
            , ("Configs Editor" , editFiles "./" [". "
                                                 ,"README.md"
                                                 ,"install.zsh"] )
            , ("XMonad Configs" , spawnConfigSession "XMonad2"   ) -- "WM/XMonad/lib/XMonad"
            , ("Xorg Configs"   , spawnConfigSession "Xorg"      ) -- (xinitrc,
            , ("VIM Configs"    , spawnConfigSession "vimrc"     )
            , ("Shell Configs"  , spawnConfigSession "Shells"    )
            , ("i3 Configs"     , editFiles "./WM/i3/" ["config"]) ]
        , tdKeyBindings     = []
        }
  ]
  where spawnConfigSession s = "gvim -role 'configEditor' -c ':OpenSession " ++ s ++ "<CR>'"
        --editFiles :: String -> [String] -> String
        editFiles d fs       = "gvim -role 'configEditor' -p" ++ concatMap (d++) fs

