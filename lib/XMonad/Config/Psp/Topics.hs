module XMonad.Config.Psp.Topics
( -- * A number of functions to be used xmonad.hs
  myTopics
, myTopicKeys
, myNumberedTopics
, myTopicConfig
, myTopicDirs
, myTopicActions
, myTopicLayoutHook
, myDefaultTopic
, spawnShell
, wsGo
) where

import qualified Data.Map as M                  ( Map(..) )

import           XMonad                         ( ManageHook(..),spawn,X(..) )
import           XMonad.Util.WindowProperties   ( Property(..) )
import           XMonad.Actions.RotSlaves       (rotSlavesUp,rotSlavesDown)
import qualified XMonad.Actions.TopicSpace as TS (currentTopicDir,def,Dir,Topic,TopicConfig(..))
import           XMonad.Actions.TopicSpace      ( (>*>) )

import           XMonad.Actions.TopicDefinitions

import           XMonad.Config.Psp.Utils
import           XMonad.Config.Psp.Configs
import           XMonad.Config.Psp.Layouts      (myStandardLayout,myTopicLayoutHook)
import           XMonad.Config.Psp.Scratchpads  (namedScratchpadAction, myScratches)

import           XMonad.Util.WindowProperties
import           XMonad.Actions.WindowGo

--spawn' :: String -> X()
--spawn' app = currentTopicDir myTopicConfig >>= (spawnIn app)

spawn' :: String -> X()
spawn' app = TS.currentTopicDir myTopicConfig >>= (spawner app)
    where spawner :: String -> String -> X()
          spawner app' dir = spawn $ "cd '" ++ dir ++ "' && '" ++ app' ++ "'"

-- | As defined in documentation for the module X.A.GridSelect.
--   Spawn a shell in the dir that is bound to current topic.
spawnShell :: X ()
spawnShell = TS.currentTopicDir myTopicConfig >>= spawnShellIn

myTopics        = topics myTopicDefs            :: [TS.Topic]
myTopicKeys     = topicEZKeys myTopicDefs myTopicConfig :: [(String, X())] -> [(String, X())]
myNumberedTopics= numberedTopics myTopicDefs    :: [TS.Topic]
myTopicDirs     = topicDirs myTopicDefs         :: M.Map TS.Topic TS.Dir
myDefaultTopic  = head myTopics                 :: TS.Topic
myTopicActions  = topicActions myTopicDefs      :: M.Map TS.Topic (X())

wsGo = goToSelectedWS myTopicConfig

myTopicConfig :: TS.TopicConfig
myTopicConfig = TS.def
  { TS.topicDirs           = myTopicDirs
  , TS.defaultTopicAction  = const $ return ()
  , TS.defaultTopic        = myDefaultTopic
  , TS.topicActions        = myTopicActions
  }
myTopicDefs :: TopicDefinitions
myTopicDefs =
  [
    TopicDefinition
        { tdName            = "1:main"  -- Workspace name
        , tdDir             = "/home/psp"-- X.A.TopicSpace directory
        , tdAction          = -- X () action to spawn when workspace is chosen
                              return ()
                              --runOrRaise "urxvtc" (propertyToQuery (Resource "urxvt"))  >*> 2
        , tdActionOnStartup = False     -- Should action be run on XMonad start?
        , tdActionOnFocus   = False     -- Should it be run when ws is selected?
        , tdHidden          = False     -- Should the workspace be hidden
        , tdBoundApps       = []        -- X Property of apps that should spawn on this workspace
        , tdMenuApps        = -- Apps that can be launched via ws menu
                              [ ("Terminal"  , taTerminal progs)
                              , ("PDF Viewer", taPDF progs     )
                              , ("Editor"    , taEditor progs  ) ]
        , tdKeyBindings     = -- KeyBindings that are bound to this workspace
                              [ ("M-z", spawnShell ) ]
        -- , tdBoundLayout     = myStandardLayout
                                -- Possibly @ModifiedLayout a l@, unsure of actual type, though
        --                      -- For now just make a custom a one.
        --                      -- See LayoutHook in xmonad.hs or compose one (possibly with
        --                      -- examples from PSP.Layouts).
        --                        myStandardLayout
        }
  , TopicDefinition
        { tdName            = "2:misc"
        , tdDir             = "/home/psp"
        , tdHidden          = False
        , tdAction          = return ()
        , tdActionOnStartup = False
        , tdActionOnFocus   = False
        , tdBoundApps       = []
        , tdMenuApps        = [] -- Should probably be a shit load here...
        , tdKeyBindings     = []
        -- , tdBoundLayout     = myStandardLayout
        }
  , TopicDefinition
        { tdName            = "3:organiser"
        , tdAction          = spawn' $ taMail progs
        , tdActionOnStartup = True
        , tdActionOnFocus   = False
        , tdHidden          = False
        , tdDir             = "/home/psp/mail"
        , tdBoundApps       = [ ClassName "Thunderbird"
                              , Resource "mutt" ]
        , tdMenuApps        = [ ("Mail and Calendar", taMailCalendar progs     )
                              , ("Mail (mutt)"      , taMail progs             )
                              , ("Notes"            , "urxvt -cd '/home/psp/notes' -e vim -c ':e .'") ]
        , tdKeyBindings     = []
        -- , tdBoundLayout     = myStandardLayout
        }
  , TopicDefinition
        { tdName            = "4:dev"
        , tdAction          = spawn' "gvim" >>
                              spawnIn "/home/psp/dev" "urxvt -role 'editorterm"
        , tdActionOnStartup = True
        , tdActionOnFocus   = False
        , tdHidden          = False
        , tdDir             = "/home/psp/dev"
        , tdBoundApps       = [ ClassName "Gvim"
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
        , tdMenuApps        = [ ("GVIM"          , "gvim --role 'dev-gvim'"           )
                              , ("Netbeans"      , "netbeans"                         )
                              , ("Eclipse"       , "eclipse"                          )
                              , ("Sublime Text 2", "subl"                             )
                              , ("Notepadqq"     , "notepadqq"                        )
                              , ("IntelliJ"      , "intellij-idea-ultimate-edition"   )
                              , ("PDF Viewer"    , taPDF progs                       )
                              , ("PDF Viewer"    , "mupdf"                            ) ]
        , tdKeyBindings     = [ ("M-<Tab>"       , rotSlavesDown   )
                              , ("M-S-<Tab>"     , rotSlavesUp     )
                              , ("M-t"           , namedScratchpadAction myScratches "editorterm")] -- won't need to push stuff into tiling here.
        -- , tdBoundLayout     = myStandardLayout
        }
  , TopicDefinition
        { tdName            = "5:www"
        , tdAction          = spawn $ taBrowser progs
        , tdActionOnStartup = False
        , tdActionOnFocus   = False
        , tdHidden          = False
        , tdDir             = "/home/psp/downloads"
        , tdBoundApps       = [ ClassName "Firefox"
                              , ClassName "Midori"
                              , ClassName "Google-chrome-stable"
                              , ClassName "Chromium"
                              , ClassName "Opera"
                              , ClassName "Vimprobable"
                              , ClassName "Xombrero"
                              , Role "browser" ]
        , tdMenuApps        = [ ("Midori"     , "midori"        )
                              , ("Firefox"    , "firefox"       )
                              , ("Chrome"     , "chrome"        )
                              , ("Chromium"   , "chromium"      )
                              , ("Opera"      , "opera"         )
                              , ("Vimprobable", "vimprobable"   )
                              , ("Xombrero"   , "xombrero"      ) ]
        , tdKeyBindings     = [ ("M-b w", spawn' $ taBrowserWin progs   )
                              , ("M-b t", spawn' $ taBrowserTab progs   )
                              , ("M-b p", spawn' $ taPBrowserWin progs  ) ]
        -- , tdBoundLayout     = myStandardLayout
        }
  , TopicDefinition
        { tdName            = "6:chat"
        , tdAction          = spawn' "pidgin" >> spawn "apulse32 skype"
        , tdActionOnStartup = True
        , tdActionOnFocus   = False
        , tdHidden          = False
        , tdDir             = "/home/psp/downloads"
        , tdBoundApps       = [ ClassName "Skype"
                              , ClassName "Pidgin"
                              , ClassName "Xchat" ]
        , tdMenuApps        = [ ("Skype" , "skype" )
                              , ("Pidgin", "pidgin") ]
        , tdKeyBindings     = []
        -- , tdBoundLayout     = myStandardLayout
        }
  , TopicDefinition
        { tdName            = "7:vms"
        , tdAction          = spawn' "virtualbox"
        , tdActionOnStartup = False
        , tdActionOnFocus   = False
        , tdHidden          = False
        , tdDir             = "/home/psp/vms"
        , tdBoundApps       = [ ClassName "VirtualBox" ]
        , tdMenuApps        = [ ("VirtualBox", "virtualbox"                  )
                              , ("Win7 VM"   , "virtualbox --startvm win7"   )
                              , ("fedbox VM" , "virtualbox --startvm fedbox" ) ]
        , tdKeyBindings     = []
        -- , tdBoundLayout     = myStandardLayout
        }
  , TopicDefinition
        { tdName            = "8:media"
        , tdAction          = spawn' "gimp"
        , tdActionOnStartup = False
        , tdActionOnFocus   = False
        , tdHidden          = False
        , tdDir             = "/home/psp/images"
        , tdBoundApps       = [ ClassName "Gimp"
                              , ClassName "Lives"
                              , ClassName "Pitivi" ]
        , tdMenuApps        = [ ("The Gimp", "gimp"  )
                              , ("Pitivi"  , "pitivi")
                              , ("LiVES"   , "lives" ) ]
        , tdKeyBindings     = []
        -- , tdBoundLayout     = myStandardLayout
        }
  , TopicDefinition
        { tdName            = "server"
        , tdAction          = spawnShell >> spawn "beastmouse"
        , tdActionOnStartup = False
        , tdActionOnFocus   = False
        , tdHidden          = True
        , tdDir             = "/home/psp/shares"
        , tdBoundApps       = []
        , tdMenuApps        = []--("M-t a", spawn' {- script to add a torrent file to server -})
        , tdKeyBindings     = [ ("M-S-m p", spawn "ssh beast 'mpc toggle'")
                              , ("M-S-m s", spawn "ssh beast 'mpc stop'"  )
                              , ("M-S-m f", spawn "ssh beast 'mpc next'"  )
                              , ("M-S-m b", spawn "ssh beast 'mpc prev'"  ) ]
        -- , tdBoundLayout     = myStandardLayout
        }
  , TopicDefinition
        { tdName            = "remotes"
        , tdAction          = spawnShell >*> 5 -- possibly terminals to five most used connections
        , tdActionOnStartup = False
        , tdActionOnFocus   = False
        , tdHidden          = True
        , tdDir             = "/home/psp/shares"
        , tdBoundApps       = [ Resource "xfreerdp"
                              , Resource "qsynergy"
                              , ClassName "Teamviewer.exe" `And` (Not $ Resource "Actual window") ]
        , tdMenuApps        = [ ("XFreeRDP"   , "xfreerdp"    )
                              , ("QSynergy"   , "qsynergy"    )
                              , ("Teamviewer" , "teamviewer"  ) ]
        , tdKeyBindings     = []
        -- , tdBoundLayout     = myStandardLayout
        }
  , TopicDefinition
        { tdName            = "configs"
        , tdAction          = return ()
        , tdActionOnStartup = False
        , tdActionOnFocus   = False
        , tdHidden          = False
        , tdDir             = "/home/psp/dev/Configs"
        , tdBoundApps       = []
        , tdMenuApps        = []
        , tdKeyBindings     = []
        -- , tdBoundLayout     = myStandardLayout
        }
  ]

-- | From: https://mail.haskell.org/pipermail/xmonad/2012-August/012880.html
--visibleWorkspaces :: X [WorkspaceId]
--visibleWorkspaces = do
--    ws <- gets windowset
--    return $ map W.tag $ W.current ws:(W.visible ws)
