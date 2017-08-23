module QubesTopics where

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
