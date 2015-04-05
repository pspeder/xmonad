module PSP.Topics
( -- * A number of functions to be used xmonad.hs
  myTopics
, myTopicKeys
, myNumberedTopics
, myTopicConfig
, myTopicDirs
, myTopicActions
, myTopicLayoutHook
, myDefaultTopic
, myManageHook
, spawnShell
) where

-- These imports are pretty much a minimum requirement.
import XMonad (ManageHook(..),spawn,X(..))
import qualified Data.Map as M (Map(..))
import XMonad.Util.WindowProperties (Property(..))
import PSP.Topics.Utils   -- Constructors and helper functions for a cleaner Topics.hs file
import PSP.Utils      (spawnIn,spawnShellIn)
-- Some actions
import XMonad.Actions.RotSlaves (rotSlavesUp,rotSlavesDown)
import XMonad.Actions.TopicSpace ((>*>),currentTopicDir,defaultTopicConfig,Dir,Topic,TopicConfig(..))

import PSP.Constants        -- Will be replaced be a ProgSetup record.
import PSP.Layouts    (myStandardLayout,myTopicLayoutHook)
import PScratchpads

myTopicDefs :: TopicDefinitions
myTopicDefs =
  [
    TopicDefinition
        { tdName            = "1:main"  -- Workspace name
        , tdAction          = -- X () action to spawn when workspace is chosen
                              spawnShell >*> 2
        , tdActionOnStartup   = False     -- Should action be run on XMonad start?
        , tdActionOnFocus   = False     -- Should it be run when ws is selected?
        , tdHidden          = False     -- Should the workspace be hidden
        , tdDir             = "/home/psp"-- X.A.TopicSpace directory
        , tdBoundApps       = []        -- X Property of apps that should spawn on this workspace
        , tdMenuApps        = -- Apps that can be launched via ws menu
                              [ ("Terminal"  , myTerminal  )
                              , ("PDF Viewer", myPDFViewer )
                              , ("Editor"    , myEditor    ) ]
        , tdKeyBindings     = -- KeyBindings that are bound to this workspace
                              [ ("M-z", spawnShell ) ]
        --, tdBoundLayout     = -- Possibly @ModifiedLayout a l@, unsure of actual type, though
        --                      -- For now just make a custom a one.
        --                      -- See LayoutHook in xmonad.hs or compose one (possibly with
        --                      -- examples from PSP.Layouts).
        --                        myStandardLayout
        }
  , TopicDefinition
        { tdName            = "2:misc"
        , tdAction          = return ()
        , tdActionOnStartup   = False
        , tdActionOnFocus   = False
        , tdHidden          = False
        , tdDir             = "/home/psp"
        , tdBoundApps       = []
        , tdMenuApps        = [] -- Should probably be a shit load here...
        , tdKeyBindings     = []
        --, tdBoundLayout     = myStandardLayout
        }
  , TopicDefinition
        { tdName            = "3:organiser"
        , tdAction          = spawn' myMailClient
        , tdActionOnStartup = True
        , tdActionOnFocus   = False
        , tdHidden          = False
        , tdDir             = "/home/psp/mail"
        , tdBoundApps       = [ ClassName "Thunderbird"
                              , Resource "mutt" ]
        , tdMenuApps        = [ ("Mail and Calendar", "thunderbird"             )
                              , ("Mail (mutt)"      , "mutt"                    )
                              , ("Notes"            , "urxvt -cd '/home/psp/notes' -e vim -c ':e .'") ]
        , tdKeyBindings     = []
        --, tdBoundLayout     = myStandardLayout
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
                              , ("PDF Viewer"    , myPDFViewer                        )
                              , ("PDF Viewer"    , "mupdf"                            ) ]
        , tdKeyBindings     = [ ("M-<Tab>"       , rotSlavesDown   )
                              , ("M-S-<Tab>"     , rotSlavesUp     )
                              , ("M-t"           , namedScratchpadAction myScratches "editorterm")] -- won't need to push stuff into tiling here.
        --, tdBoundLayout     = myStandardLayout
        }
  , TopicDefinition
        { tdName            = "5:www"
        , tdAction          = spawn myBrowser
        , tdActionOnStartup   = False
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
        , tdKeyBindings     = [ ("M-n"  , spawn' myBrowserNewWindow  )
                              , ("M-S-n", spawn' myBrowserPrivateWin ) ]
        --, tdBoundLayout     = myStandardLayout
        }
  , TopicDefinition
        { tdName            = "6:chat"
        , tdAction          = spawn' "pidgin" >> spawn "apulse32 skype"
        , tdActionOnStartup   = True
        , tdActionOnFocus   = False
        , tdHidden          = False
        , tdDir             = "/home/psp/downloads"
        , tdBoundApps       = [ ClassName "Skype"
                              , ClassName "Pidgin"
                              , ClassName "Xchat" ]
        , tdMenuApps        = [ ("Skype" , "skype" )
                              , ("Pidgin", "pidgin") ]
        , tdKeyBindings     = []
        --, tdBoundLayout     = myIMLayout
        }
  , TopicDefinition
        { tdName            = "7:vms"
        , tdAction          = spawn' "virtualbox"
        , tdActionOnStartup   = False
        , tdActionOnFocus   = False
        , tdHidden          = False
        , tdDir             = "/home/psp/vms"
        , tdBoundApps       = [ ClassName "VirtualBox" ]
        , tdMenuApps        = [ ("VirtualBox", "virtualbox"                  )
                              , ("Win7 VM"   , "virtualbox --startvm win7"   )
                              , ("fedbox VM" , "virtualbox --startvm fedbox" ) ]
        , tdKeyBindings     = []
        --, tdBoundLayout     = myStandardLayout
        }
  , TopicDefinition
        { tdName            = "8:media"
        , tdAction          = spawn' "gimp"
        , tdActionOnStartup   = False
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
        --, tdBoundLayout     = myStandardLayout
        }
  , TopicDefinition
        { tdName            = "server"
        , tdAction          = spawnShell >> spawn "beastmouse"
        , tdActionOnStartup   = False
        , tdActionOnFocus   = False
        , tdHidden          = True
        , tdDir             = "/home/psp/shares"
        , tdBoundApps       = []
        , tdMenuApps        = []--("M-t a", spawn' {- script to add a torrent file to server -})
        , tdKeyBindings     = [ ("M-S-m p", spawn "ssh beast 'mpc toggle'")
                              , ("M-S-m s", spawn "ssh beast 'mpc stop'"  )
                              , ("M-S-m f", spawn "ssh beast 'mpc next'"  )
                              , ("M-S-m b", spawn "ssh beast 'mpc prev'"  ) ]
        --, tdBoundLayout     = myStandardLayout
        }
  , TopicDefinition
        { tdName            = "remotes"
        , tdAction          = spawnShell >*> 5 -- possibly terminals to five most used connections
        , tdActionOnStartup   = False
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
        --, tdBoundLayout     = myStandardLayout
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
        --, tdBoundLayout     = myStandardLayout
        }
  ]

--spawn' :: String -> X()
--spawn' app = currentTopicDir myTopicConfig >>= (spawnIn app)

spawn' :: String -> X()
spawn' app = currentTopicDir myTopicConfig >>= (spawner app)
    where spawner :: String -> String -> X()
          spawner app' dir = spawn $ "cd '" ++ dir ++ "' && '" ++ app' ++ "'"

-- | As defined in documentation for the module X.A.GridSelect.
--   Spawn a shell in the dir that is bound to current topic.
spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

myTopics        = topics myTopicDefs            :: [Topic]
myTopicKeys     = topicEZKeys myTopicDefs myTopicConfig :: [(String, X())] -> [(String, X())]
myManageHook    = topicManageHook myTopicDefs   :: [Property]
                                                -> [Property]
                                                -> [Property]
                                                -> [Property]
                                                -> [Property]
                                                -> [(Property, Topic)]
                                                -> ManageHook
myNumberedTopics= numberedTopics myTopicDefs    :: [Topic]
myTopicDirs     = topicDirs' myTopicDefs        :: M.Map Topic Dir
myDefaultTopic  = head myTopics                 :: Topic
myTopicActions  = topicActions' myTopicDefs     :: M.Map Topic (X())

myTopicConfig :: TopicConfig
myTopicConfig = defaultTopicConfig
  { topicDirs           = myTopicDirs
  , defaultTopicAction  = const $ return ()
  , defaultTopic        = myDefaultTopic
  , topicActions        = myTopicActions
  }

