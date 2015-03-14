module PSP.Topics
( myTopics
, myNumberedTopics
, myTopicConfig
, myTopicDirs
, myTopicActions
, myDefaultTopic
, myTopicKeys
, myManageHook
, myTopicLayoutHook
, spawnSelected'
, spawnShell
) where

import XMonad
import XMonad.ManageHook
import Data.Char (isDigit)
import Data.List (sortBy, stripPrefix)
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.RotSlaves (rotSlavesUp,rotSlavesDown)
import XMonad.Actions.TopicSpace
import XMonad.Actions.GridSelect
import XMonad.Actions.PerWorkspaceKeys
import XMonad.Layout.Fullscreen (FullscreenFull(..), fullscreenFull)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.Spacing
import XMonad.Layout.WindowNavigation
import XMonad.Hooks.ManageDocks (avoidStruts, ToggleStruts(..))
import XMonad.Layout.Maximize
import XMonad.Layout.BoringWindows (boringWindows)
import           XMonad.Layout.Tabbed -- (tabbedAlways,shrinkText,defaultTheme) -- Tabbed layout
import XMonad.Layout.Renamed
import XMonad.Layout.Accordion
import           XMonad.Layout.IM                                       -- For multi-window apps (Gimp/Pidgin)
import XMonad.Layout.Reflect
import XMonad.Util.Run
import XMonad.Actions.WithAll (sinkAll)
import XMonad.Util.WindowProperties
import XMonad.Layout.NoBorders
import qualified Data.Map as M
import PSP.Constants

myTopicDefs :: TopicDefinitions
myTopicDefs =
  [
    TopicDefinition
        { tdName            = "1:main"              -- Workspace name
        , tdAction          = spawnShell >*> 2      -- X () action to spawn when workspace is chosen
        , tdStartupAction   = False                 -- Should action be run on XMonad start?
        , tdHidden          = False                 -- Should the workspace be hidden
        , tdDir             = "~"                   -- X.A.TopicSpace directory
        , tdBoundApps       = []                    -- X Property of apps that should always spawn on this workspace
        , tdMenuApps        =                       -- Apps that can be launched via ws menu
            [ ("Terminal",      myTerminal  )
            , ("PDF Viewer",    myPDFViewer )
            , ("Editor",        myEditor    )
            ]
        , tdKeyBindings     =                       -- KeyBindings that are bound to this workspace
            [ ("M-z", spawnShell ) ]
        --, tdBoundLayout     = myStandardLayout      -- Layout hook specific for this layout
        }
  , TopicDefinition
        { tdName            = "2:misc"
        , tdAction          = return ()
        , tdStartupAction   = False
        , tdHidden          = False
        , tdDir             = "~"
        , tdBoundApps       = []
        , tdMenuApps        =
            [ -- Should probably be a shit load here...
            ]
        , tdKeyBindings     = [ ("M-z", spawn "notify-send 'Det virker'") ]
        --, tdBoundLayout     = myStandardLayout
        }
  , TopicDefinition
        { tdName            = "3:organiser"
        , tdAction          = spawn myMailClient
        , tdStartupAction   = True
        , tdHidden          = False
        , tdDir             = "~/mail"
        , tdBoundApps       = [ClassName "Thunderbird", Resource "mutt"]
        , tdMenuApps        =
            [ ("Mail and Calendar", "thunderbird"     )
            , ("Mail (mutt)",       "mutt"            )
            , ("Notes",             "urxvt -cd ~/notes -e vim"  )
            ]
        , tdKeyBindings     =
            [ ("M-z", spawn $ "notify-send " ++ "\"noget\"") ]
        --, tdBoundLayout     = myStandardLayout
        }
  , TopicDefinition
        { tdName            = "4:dev"
        , tdAction          = spawn "gvim"
        , tdStartupAction  = False
        , tdHidden          = False
        , tdDir             = "~/dev"
        , tdBoundApps       = [ ClassName "Gvim"
                              , ClassName "Netbeans IDE 8.0.2"
                              , ClassName "Eclipse"
                              , ClassName "Tea"
                              , ClassName "Gedit"
                              , ClassName "sublime_text"
                              , ClassName "Notepadqq"
                              , ClassName "jetbrains-idea"
                              , ClassName "jetbrains-android-studio"
                              , ClassName "Mousepad"]
        , tdMenuApps        =
            [ ("GVIM",              "gvim --role 'dev-gvim'"          )
            , ("Netbeans",          "netbeans"                        )
            , ("Eclipse",           "eclipse"                         )
            , ("Sublime Text 2",    "subl"                            )
            , ("Notepadqq",         "notepadqq"                       )
            , ("IntelliJ",          "intellij-idea-ultimate-edition"  )
            , ("PDF Viewer",        myPDFViewer                       )
            , ("PDF for Edit",      "impressive -B 0 --windowed --nologo --noclicks --wrap --nowheel")
            ]
        , tdKeyBindings      =
            [ ("M-j",     rotSlavesDown   )
            , ("M-k",     rotSlavesUp     )
            ]
        --, tdBoundLayout     = myStandardLayout
        }
  , TopicDefinition
        { tdName            = "5:www"
        , tdAction          = spawn myBrowser
        , tdStartupAction  = False
        , tdHidden          = False
        , tdDir             = "~/downloads"
        , tdBoundApps       = [ ClassName "Firefox"
                              , ClassName "Midori"
                              , ClassName "Google-chrome-stable"
                              , ClassName "Chromium"
                              , ClassName "Opera"
                              , ClassName "Vimprobable"
                              , ClassName "Xombrero"
                              , Role "browser" ]
        , tdMenuApps        =
            [ ("Firefox",           "firefox"     )
            , ("Chrome",            "chrome"      )
            , ("Chromium",          "chromium"    )
            , ("Opera",             "opera"       )
            , ("Vimprobable",       "vimprobable" )
            , ("Xombrero",          "xombrero"    )
            ]
        , tdKeyBindings     =
            [ ("M-n",               spawn myBrowserNewWindow    )
            , ("M-S-n",             spawn myBrowserPrivateWin   )
            ]
        --, tdBoundLayout     = myStandardLayout
        }
  , TopicDefinition
        { tdName            = "6:chat"
        , tdAction          = spawn "pidgin" >> spawn "skype"
        , tdStartupAction  = True
        , tdHidden          = False
        , tdDir             = "~/downloads"
        , tdBoundApps       = [ ClassName "Skype"
                              , ClassName "Pidgin"
                              , Resource "Xchat"]
        , tdMenuApps        =
            [ ("Skype",             "skype" )
            , ("Pidgin",            "pidgin")
            ]
        , tdKeyBindings     = []
        --, tdBoundLayout     = myIMLayout
        }
  , TopicDefinition
        { tdName            = "7:vms"
        , tdAction          = spawn "virtualbox"
        , tdStartupAction   = False
        , tdHidden          = False
        , tdDir             = "~/shares"
        , tdBoundApps       = [ ClassName "VirtualBox" ]
        , tdMenuApps        =
            [ ("VirtualBox",    "virtualbox"                  )
            , ("Win7 VM",       "virtualbox --startvm win7"   )
            , ("fedbox VM",     "virtualbox --startvm fedbox" )
            ]
        , tdKeyBindings     = []
        --, tdBoundLayout     = myStandardLayout
        }
  , TopicDefinition
        { tdName            = "8:media"
        , tdAction          = spawn "gimp"
        , tdStartupAction   = False
        , tdHidden          = False
        , tdDir             = "~/images"
        , tdBoundApps       = [ ClassName "Gimp"
                              , ClassName "Lives"
                              , ClassName "Pitivi" ]
        , tdMenuApps        =
            [ ("The Gimp", "gimp"  )
            , ("Pitivi",   "pitivi")
            , ("LiVES",    "lives" )
            ]
        , tdKeyBindings     = []
        --, tdBoundLayout     = myStandardLayout
        }
  , TopicDefinition
        { tdName            = "server"
        , tdAction          = spawnShell >> spawn "beastmouse"
        , tdStartupAction  = False
        , tdHidden          = True
        , tdDir             = "~/shares"
        , tdBoundApps       = []
        , tdMenuApps        = []--("M-t a", spawn {- script to add a torrent file to server -})
        , tdKeyBindings     = [ ("M-m p", spawn "ssh beast 'mpc toggle'"                )
                              , ("M-m s", spawn "ssh beast 'mpc stop'"                  )
                              , ("M-m f", spawn "ssh beast 'mpc next'"                  )
                              , ("M-m b", spawn "ssh beast 'mpc prev'"                  )
                              ]
        --, tdBoundLayout     = myStandardLayout
        }
  , TopicDefinition
        { tdName            = "remotes"
        , tdAction          = spawnShell >*> 5 -- possibly terminals to five most used connections
        , tdStartupAction  = False
        , tdHidden          = True
        , tdDir             = "~/shares"
        , tdBoundApps       = [ Resource "xfreerdp"
                              , Resource "qsynergy"
                              , ClassName "Teamviewer.exe" `And` (Not $ Resource "Actual window") ]
        , tdMenuApps        =
            [ ("XFreeRDP"       ,"xfreerdp"    )
            , ("QSynergy"       ,"qsynergy"    )
            , ("Teamviewer"     ,"teamviewer"  )
            ]
        , tdKeyBindings     = []
        --, tdBoundLayout     = myStandardLayout
        }
  , TopicDefinition
        { tdName            = "configs"
        , tdAction          = spawnSelected' configs
        , tdStartupAction  = False
        , tdHidden          = False
        , tdDir             = "~/dev/Configs"
        , tdBoundApps       = []
        , tdMenuApps        = configs
        , tdKeyBindings     = []
        --, tdBoundLayout     = myStandardLayout
        }
  ]
    where configs = [("zsh", "")]


data TopicDefinition = TopicDefinition
                     { tdName        :: !Topic
                     , tdAction      :: !(X ())
                     , tdStartupAction :: !Bool
                     , tdHidden      :: !Bool
                     , tdDir         :: !Dir
                     , tdMenuApps    :: ![(String, String)] -- Pretty name and exec for 2D menu
                     , tdBoundApps   :: ![Property]        -- Some window property for ManageHook
                     , tdKeyBindings :: ![(String, X())] -- [(key, app)]
                     --, tdBoundLayout :: ManageHook
                     }

type TopicDefinitions = [TopicDefinition]

--myTopics                = TU.topics myTopicDefs
-- |List of topic names
myTopics :: [Topic]
myTopics = map (\x -> tdName x) myTopicDefs

myNumberedTopics :: [Topic]
myNumberedTopics = foldr (\x acc -> if isDigit $ head x then x:acc else acc) [[]] myTopics

myDefaultTopic :: Topic
myDefaultTopic = head myTopics

--myTopicDirs             = TU.topicDirs myTopicDefs
-- |Map, tying together topic names with their directories
--  (for use in TopicConfig)
--myTopicDirs :: M.Map (String,Dir)
myTopicDirs = M.fromList $ map (\x -> (tdName x, tdDir x)) myTopicDefs

--myTopicActions          = TU.topicActions myTopicDefs
-- |Map from topic name to topic action
--  (for use in TopicConfig)
--myTopicActions :: M.Map (String, X ())
myTopicActions = M.fromList $ map (\x -> (tdName x, tdAction x)) myTopicDefs


--http://groups.google.com/group/fa.haskell/browse_thread/thread/daa11e5471402149?pli=1
replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace _ _ []            = []
replace old new xs@(y:ys) = case stripPrefix old xs
                            of Nothing  -> y : replace old new ys
                               Just ys' -> new ++ replace old new ys'

--myTopicKeys             = TU.topicKeyBinds myTopicDefs
--                       ++ TU.topicMenuKeyBind (myTopicDefs, myGSConfig, myTopicMenuKey)
--topicMenuKeyBind :: TopicDefinitions -> (String, X ())
--            --[( "M-" ++ [k], windows $ W.view t ) | t <- myNumberedTopics,
              --                                             k <- show $ take (length myNumberedTopics) [1..] ] ++
              --[( "M-S-" ++ k, windows . W.shift t ) | t <- myNumberedTopics, k <- map (\n -> take 1 n) myNumberedTopics ] ++
-- TODO : Take as argument the other keys and make keybindings in there that overlap with topicAppsKeys be bound on corresponding workspaces and the default ("", Action) be the binding in regular keys
myTopicKeys :: [(String, X ())]
myTopicKeys = ( ("M-m", bindOn topicsApps):(topicShifts ++ keyBinds) )
    where
        topicsApps :: [(String, X())]
        topicsApps = map (\TopicDefinition {tdName = n, tdMenuApps = as} -> (n, spawnSelected' as)) myTopicDefs

        topicShifts :: [(String, X ())]
        topicShifts = [("M-" ++ m ++ k, windows $ f i)
                        | (i, k) <- zip (myNumberedTopics) $ map show $ take (length myNumberedTopics) [1..]
                        , (f, m) <- [(W.greedyView, ""), (W.shift, "S-")]]

        keyBinds :: [(String, X())]
        keyBinds = (shrinkAndBind [] sortedTopicAppsKeys)

        -- | Shrink a list from 'sortedTopicAppsKeys' into a list of keybindings to be used in 'additionalKeysP'
        shrinkAndBind :: [(String, [(Topic, X())])] -- ^ Accumulator / Already reduced list
                      -> [(String, [(Topic, X())])] -- ^ Sorted list of tuples of (key string, [(Topic name, X() Action)])
                      -> [(String, X())]            -- ^ Duplicates reduced in sorted list of tuples of (key string, bindOn [(Topic name, X() Action)])
        shrinkAndBind acc []     = map (\(k, nas) -> (k, bindOn nas)) acc --base case: return key bindings
        shrinkAndBind acc (x:[]) = shrinkAndBind (x:acc) []
        shrinkAndBind acc ((k1, nas1):(k2, nas2):ks) = if k1 == k2
                                                       then shrinkAndBind ((k1,nas1++nas2):acc)            ks
                                                       else shrinkAndBind       ((k1,nas1):acc) ((k2,nas2):ks)

        -- | Manipulate a list of 'TopicDefinition' into a sorted list of tuples with el. 1 being key and el. 2 being tuples for X.A.BindOn
        sortedTopicAppsKeys :: [( String, [(String, X())] )]
        sortedTopicAppsKeys =
            let -- | Determines if first argument is lexigraphically smaller than the second.
                smaller :: String -> String -> Bool
                smaller s1 s2 | len1 /= len2         = (len1 < len2)
                              | otherwise            = (s1 < s2)
                    where (len1, len2) = (length s1, length s2)

                sortBinds :: (String, [(Topic, X())]) -> (String, [(Topic, X())]) -> Ordering
                sortBinds (k1,_) (k2,_) | k1 == k2        = EQ
                                        | k1 `smaller` k2 = LT
                                        | otherwise       = GT

                sortKeys :: [(String, [(Topic, X())])] -> [(String, [(Topic, X())])]
                sortKeys = sortBy sortBinds

            in sortKeys $ concat $ foldr (\TopicDefinition {tdName=n, tdKeyBindings=ks} acc
                                            -> (map (\(k,a) -> (k, [(n,a)])) ks):acc)
                                         [] myTopicDefs

{-
          keyBinds = map (\(k', tas') -> (k', bindOn tas')) $
                     foldr (\TopicDefinition { tdName = n, tdKeyBindings = ks } acc ->
                                map (\(k, a) -> let (xs, ((_, tas):ys)) = (span (\(key, _) -> key /= k) acc)
                                                 in (xs ++ ((k, (n,a):tas):ys)) ) ks) [] myTopicDefs
          keyBinds = map (\(final_key, final_topicBoundApps) -> (final_key, bindOn final_topicBoundApps)) $
                     foldr (\TopicDefinition {tdName=n, tdKeyBindings=ks} acc ->
                        map (\(key, app) ->
                            let funny (newAccBeg, (teK, teB):[]) = newAccBeg ++ [(key, (n, app):teB)]
                                funny (newAccBeg, newAccEnd)
                                    | null newAccBeg = let teB = (snd $ head newAccEnd)
                                                           accEnd = drop 1 newAccEnd
                                                       in (key, (n, app):teB):accEnd
                                    | null newAccEnd = newAccBeg ++ [(key, [(n, app)])]
                                    | otherwise      = let teB = (snd $ head newAccEnd)
                                                           accEnd = drop 1 newAccEnd
                                                       in newAccBeg ++ [(key, (n, app):teB)] ++ accEnd
                            in  funny $ span (\(k, _) -> key /= k) acc) ks) [] myTopicDefs
 newAccBeg (newAccBeg::[(String, [(Topic, X())])], tmpNewAccEnd)::[(String, [(Topic, X())])]) = span (\(k, _) -> k /= key) acc
                                newAcc = if null ((teK, teB):tmpNewAccEnd)
                                         then newAccBeg ++ [(key, [(n, app)])]
                                         else if null tmpNewAccEnd
                                              then newAccBeg ++ [(key, ((n,app):teB))]
                                              else newAccBeg ++ ((key, ((n,app):teB)):tmpNewAccEnd)
                            in  newAcc) ks) [] myTopicDefs
          let mapF (k, a) =
                          listToX = map (\(key, binds) -> (key, bindOn binds))
                          before key = takeWhile ((key /=) . fst)
                      in  foldr (\TopicDefinition {tdName = n, tdKeyBindings = ks} acc ->
                            map () ks
                            (before ++ (newElement:after))


                              let withTopics = map (\(k, a) -> (k, n, a)) ks
                                  isInAcc (k, n, a) = if k `elem` acc
                                                      then (span (k =/) acc)
                                                      else 
          foldr (\(k,t,a) acc -> let (xs,ys) = span (k =/)
                                                tmp1 [] acc = span (k =/) 
                                                tmp = (t,a)
                                                     t $
                     foldr ( \(topic, kas) acc -> (map ( \(key, app) ->  (key, topic, app) ) kas) ++ acc ) [] $ -- should result in a long list of all topic keybindings
                     map (\TopicDefinition { tdName = n, tdKeyBindings =ks } -> let tmp = (n, ks)
                                                                                in  map ( myTopicDefs
            where appendNsAndAsToKb acc = [ (kb_nas, <-  ] ++ acc
-}

--myTopicManageHook       = TU.topicManageHook myTopicDefs
-- |ManageHook -- compile a manage hook from the following:
myManageHook :: [String]            -- ^ ts  : list of window properties to be forced into tiling (gimp hopefully)
             -> [String]            -- ^ fs  : list of window properties to float by standard floating algorithm
             -> [String]            -- ^ cfs : list of window properties to force into centered floats
             -> [String]            -- ^ ffs : list of window properties to force into full floats
             -> [(String, String)]  -- ^ mss : manual shifts [(property, workspace)]
             -> [String]            -- ^ is  : ignored window properties
             -> ManageHook          -- ^ The ManageHook to keep track of topics, floats and fullscreen apps.
myManageHook ts fs cfs ffs mss is = --(composeAll . concat $
    -- [ [ matchAny p --> doShift n | (p,n) <- mss ]
    -- , [ className =? "Gimp" --> (ask >>= doF . W.sink) ]
    -- ]) <+> 
    topicManageHook <+> (composeAll . concat $
    [ --[ matchAny ts'  --> doTile        | ts'  <- ts  ]
      [ matchAny fs'  --> doFloat       | fs'  <- fs  ]
    , [ matchAny cfs' --> doCenterFloat | cfs' <- cfs ]
    , [ matchAny ffs' --> doMyFFloat    | ffs' <- ffs ]
    , [ matchAny is'  --> doIgnore      | is'  <- is  ]
    ]) <+> (composeOne . concat $
    [ [ isFullscreen -?> doFullFloat   ]
    , [ isDialog     -?> doCenterFloat ]
    , [ return True  -?> doMaster      ] -- prevent new windows from stealing focus
    ]) where
        doTile          = (ask >>= doF . W.sink)
        wmrole          = stringProperty "WM_WINDOW_ROLE"
        doMaster        = (doF W.shiftMaster)
        doMyFFloat      = (doF W.focusDown <+> doFullFloat)
        topicManageHook = (composeAll . concat $ map (\TopicDefinition{tdName = n, tdBoundApps=as}
                        -> [ (propertyToQuery id) --> doShift n | id <- as ]) myTopicDefs)

myTopicConfig :: TopicConfig
myTopicConfig = defaultTopicConfig
  { topicDirs           = myTopicDirs
  , defaultTopicAction  = const $ return ()
  , defaultTopic        = myDefaultTopic
  , topicActions        = myTopicActions
  }

{--myTopicLayoutHook       = TU.topicLayoutHook myTopicDefs
-- | Transform a TopicDefinition-list into a ManageHook
--myTopicLayoutHook :: ManageHook
--myTopicLayoutHook = composeAll $ map (\TopicDefinition { tdName = n
                                                       , tdBoundLayout = l
                                                       , tdDir = d }
                                        -> onWorkspace n (workspaceDir d l)
                                     ) myTopicDefs
myTopicLayoutHook = avoidStruts $ renamed [CutWordsLeft 4] $ maximize $ boringWindows $ spacing 5 $
    (concat $ map (\TopicDefinition {tdName = n, tdBoundLayout = l, tdDir=d} -> onWorkspace n (workspaceDir d l)) myTopicDefs)
    myStandardLayout
-}
--gaps [(U,16), (D,16), (L,0), (R,0)]
myTopicLayoutHook = renamed [CutWordsLeft 1] $ avoidStruts $ maximize $ boringWindows $ windowNavigation
                  $ onWorkspace "1:main"  (workspaceDir "~"             $ mainLayout            )
                  $ onWorkspace "2:misc"  (workspaceDir "~"             $ renamed [CutWordsLeft 2] $ spacing 0 miscLayout  )
                  $ onWorkspace "3:organiser" (workspaceDir "~/mail"    $ organiserLayout       )
                  $ onWorkspace "4:dev"   (workspaceDir "~/dev"         $ devLayout             )
                  $ onWorkspace "5:www"   (workspaceDir "~/downloads"   $ webLayout             )
                  $ onWorkspace "6:chat"  (workspaceDir "~/downloads"   $ chatLayout            )
                  $ onWorkspace "7:vms"   (workspaceDir "~/shares"      $ vmsLayout             )
                  $ onWorkspace "8:media" (workspaceDir "~/images"      $ mediaLayout           )
                  $ onWorkspace "remotes" (workspaceDir "~/shares"      $ remotesLayout         )
                  $ onWorkspace "server"  (workspaceDir "~/srv"         $ serverLayout          )
                  $ onWorkspace "configs" (workspaceDir "~/dev/Configs" $ confLayout            )
                  $ myStandardLayout
                    where
                        mainLayout = myStandardLayout
                        miscLayout = myStandardLayout
                        organiserLayout = myStandardLayout
                        devLayout = myStandardLayout
                        webLayout = renamed [Replace"F"] $ noBorders . spacing 0 $ fullscreenFull Full
                        chatLayout = renamed [Replace"S"]
                                   $ withIM (15/100) (ClassName "Skype" `And` Role "") $ reflectHoriz
                                   $ withIM (17/100) (ClassName "Pidgin" `And` Role "buddy_list") $ reflectHoriz
                                   $ noBorders $ spacing 0 $ simpleTabbed
                        vmsLayout = myStandardLayout
                        mediaLayout = renamed [Replace"F"]
                                    $ withIM (18/100) (Role "gimp-dock")    $ reflectHoriz
                                    $ withIM (25/100) (Role "gimp-toolbox") $ reflectHoriz $ fullscreenFull Full
                        remotesLayout = myStandardLayout
                        serverLayout = myStandardLayout
                        confLayout = myStandardLayout


myStandardLayout = tiled ||| mTiled ||| simpleTabbed ||| myNoBordersFullLayout
    where tiled       = renamed [Replace "V"] $ spacing 4 $ Tall 1 (2/100) (6/10)
          mTiled      = renamed [Replace "H"] $ spacing 4 $ Mirror tiled
          myNoBordersFullLayout = renamed [Replace "F"] $ noBorders $ spacing 0 $ fullscreenFull Full
          --accordion   = renamed [Replace "Accordion"] Accordion

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ "urxvtc -cd '" ++ dir ++ "' "

-- | From: http://ixti.net/software/2013/09/07/xmonad-action-gridselect-spawnselected-with-nice-titles.html
-- Select an application to launch from X.A.GridSelect with a pp'ed name.
spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = defaultGSConfig

-- | Adapted from: http://pbrisbin.com/tags/xmonad/
matchAny :: String -> Query Bool
matchAny s = liftAny (=? s) [className, title, wmname, wmrole, resource]
    where
        liftAny p list = foldr ((<||>) . p) (return False) list
        wmname = stringProperty "WM_NAME"
        wmrole = stringProperty "WM_WINDOW_ROLE"

