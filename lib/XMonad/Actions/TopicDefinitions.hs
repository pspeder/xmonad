{-# LANGUAGE
    Rank2Types
  , NoMonomorphismRestriction
  , MonoLocalBinds
  , MultiParamTypeClasses
  , DataKinds
  , GADTs
  , FlexibleInstances
  , UndecidableInstances
  , PolyKinds
  , TypeOperators
  , FlexibleContexts
  , TypeFamilies
  #-}
module XMonad.Actions.TopicDefinitions
( TopicDefinition(..)
, TopicDefinitions
, defaultTopicDefinition
, Key(..)
, TDConfig(..)
, defaultTDConfig
, topics
, numberedTopics
, topicStartupHook
, topicDirs
, topicActions
, topicEZKeys
, topicManageHook
, ProgSetup(..)
) where

import XMonad
import XMonad.Layout
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LayoutCombinators (NewSelect(..))

import qualified XMonad.Actions.TopicSpace as TS(Dir,Topic,TopicConfig(..)
                                                ,switchTopic,topicAction
                                                ,checkTopicConfig,defaultTopicConfig)
-- Utilities
import qualified Data.Map as M                  (fromList,Map(..))
import           Data.List                      (sortBy,isSuffixOf)
import           Data.Char                      (isDigit)
import           Data.Monoid                    (mempty, mappend, All(..))
import           Graphics.X11.Types             (KeySym(..), ButtonMask(..))
import           System.Directory               (getHomeDirectory)
import           System.IO                      (FilePath)

import qualified XMonad.StackSet as W           (focusDown,greedyView,shift,shiftMaster,sink,view,Workspace(..))
import           XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace)
import           XMonad.Layout.PerWorkspace     (onWorkspace)
import           XMonad.Util.WindowProperties   (Property(..),propertyToQuery)
-- Hooks
import           XMonad.ManageHook
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.DynamicLog (PP(..),defaultPP,shorten,pad,dzenStrip,dzenColor,wrap,dzenPP,dzenEscape,dynamicLogWithPP,ppOutput)
-- Actions
import           XMonad.Actions.PerWorkspaceKeys(bindOn)
-- Own modules
import           XMonad.Config.Psp.Utils (spawnSelected',changeDir)

-- | A wrapper type to allow the use of both EZConfig keys as well as keys with types from Xlib
data Key = EZKey String | XKey (KeyMask, KeySym)

-- | A TopicDefinition holds information regarding a workspace/topic/desktop.
-- Possible TODO :
--    - Mouse bindings
--    - Scratchpads
data TopicDefinition = TopicDefinition
    { tdName           :: TS.Topic             -- ^ Identifier
    , tdDir            :: TS.Dir               -- ^ Directory the spawning shell should spawn from.
                                               -- If none given, use tdcDefaultDir of config.
    , tdHidden         :: Bool                 -- ^ Superflous for now. Decide whether to hide in
                                               -- loghook etc.
    , tdAction         :: (X ())               -- ^ X Action bound to topic
    , tdActionOnStartup:: Bool                 -- ^ Run the bound action on xmonad startup
    , tdActionOnFocus  :: Bool                 -- ^ Run the action when ws is displayed
    , tdBoundApps      :: [Property]           -- ^ XProperties to tie in ManageHook
    , tdMenuApps       :: [(String, String)]   -- ^ Pretty name and exec string for 2D menu
    , tdKeyBindings    :: [(String, X())]      -- ^ Keys that should enabled on ws list of (key, app)s
    }
-- | A list of type TopicDefinition
type TopicDefinitions = [TopicDefinition]

-- | A configuration of the TopicDefinitions module. Careful what you change here.
data TDConfig = TDConfig
    { tdcDefaultTopic :: TS.Topic
    , tdcDefaultDir   :: TS.Dir
    , tdcDefultAction :: (TS.Topic -> X())
    , tdcMaxHistory   :: Int
    , tdcMenuKey      :: Key
    , tdcStartupApps  :: [X()]
    , tdcProgSetup    :: ProgSetup
    }

defaultTDConfig tds = TDConfig
    { tdcDefaultTopic = head $ topics tds
    , tdcDefaultDir   = "~"
    , tdcDefultAction = const $ return ()
    , tdcMaxHistory   = 10
    , tdcMenuKey      = EZKey "M-m"
    , tdcStartupApps  = []
    , tdcProgSetup    = ProgSetup
        { taHomeDir     = "/home/psp"
        , taDefaultDir  = "/home/psp"
        , taTerminal    = "urxvtcd"
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
    }

-- | New name will probably be TopicApps
data ProgSetup = ProgSetup
    { taHomeDir     :: TS.Dir
    , taDefaultDir  :: TS.Dir
    , taTerminal    :: String
    , taPDF         :: String
    , taMail        :: String
    , taMailCalendar:: String
    , taEditor      :: String
    , taBrowser     :: String
    , taBrowserTab  :: String
    , taBrowserWin  :: String
    , taPBrowserWin :: String -- Private browser window
    , taFloats      :: [Property]
    , taCFloats     :: [Property]
    , taFullFloats  :: [Property]
    , taIgnores     :: [Property]
    , taStartup     :: [X()]
    }


--defaultTopicDefinition :: (Eq TopicDefinition, Ord TopicDefinition) => TopicDefinition
defaultTopicDefinition = TopicDefinition
    { tdName           = "1:main"
    , tdDir            = "~"
    , tdHidden         = False
    , tdAction         = return ()
    , tdActionOnStartup= False
    , tdActionOnFocus  = True
    , tdMenuApps       = [("Terminal", "urxvtc")]
    , tdBoundApps      = []
    , tdKeyBindings    = []
    }

-- Exported Functions
---------------------
-- | List of topic names
topics :: TopicDefinitions -> [TS.Topic]
topics = map (tdName)

-- |Transforms TopicDefinitions into a list containing only the
-- topics starting with a number ranging 1-9
numberedTopics :: TopicDefinitions -> [TS.Topic]
numberedTopics tds = foldr (\n acc -> if isDigit $ head n then n:acc else acc) [[]] $ topics tds

-- | Map, tying together topic names with their directories
--  (for use in TopicConfig)
topicDirs :: TopicDefinitions -> M.Map TS.Topic TS.Dir
topicDirs tds = M.fromList $ map (\t -> (tdName t, tdDir t)) tds

visibleTopicDefs :: TopicDefinitions -> TopicDefinitions
visibleTopicDefs tds = filter (not . tdHidden) tds

-- | Map from topic name to topic action
--   (for use in TopicConfig)
topicActions :: TopicDefinitions -> M.Map TS.Topic (X())
topicActions tds = M.fromList $ map (\x -> (tdName x, tdAction x)) tds

topicSpaceConf :: TDConfig -> TopicDefinitions -> TS.TopicConfig
topicSpaceConf tdc tds = TS.defaultTopicConfig
  { TS.topicDirs            = topicDirs tds
  , TS.defaultTopicAction   = tdcDefultAction tdc
  , TS.defaultTopic         = head $ topics tds
  , TS.topicActions         = topicActions tds
  , TS.maxTopicHistory      = tdcMaxHistory tdc
  }

-- | Construct a startup hook to be included in your XConfig.
--   1. checks topics with X.A.TopicSpace.checkTopicConfig
--   2. spawns the TopicDefinition actions if selected with tdActionOnStartup
--   3. spawns the apps listed in your TDConfig.ProgSetup's startupApps field
topicStartupHook :: TDConfig        -- ^ A TDConfig which holds much of the info
                                    -- for X.A.TopicSpace.TopicConfig
                 -> TopicDefinitions-- ^ [X.A.TopicSpace.Topics] to check
                 -> ProgSetup       -- ^ ProgSetup
                 -> X()
topicStartupHook tc tds ps = catchIO (TS.checkTopicConfig (topics tds) (topicSpaceConf tc tds)) >>
                            (mconcat . fmap (tdAction) . filter (tdActionOnStartup)) tds        >>
                            mconcat (taStartup ps)

-- | ManageHook -- compile a manage hook from the following:
topicManageHook :: TopicDefinitions    -- ^ tds : TopicDefinitions to bind appropriate apps
             -> [Property]          -- ^ ts  : list of window properties to be forced into tiling
             -> [Property]          -- ^ fs  : list of window properties to float by standard floating algorithm
             -> [Property]          -- ^ cfs : list of window properties to force into centered floats
             -> [Property]          -- ^ ffs : list of window properties to force into full floats
             -> [Property]          -- ^ is  : ignored window properties
             -> [(Property, TS.Topic)] -- ^ mss : manual shifts [(property, workspace)]
             -> ManageHook          -- ^ The ManageHook to keep track of topics, floats and fullscreen apps.
topicManageHook tds ts fs cfs ffs is mss =
    topicManageHook' <+> (composeAll . concat $
    [ [ propertyToQuery p   --> doShift n     | (p,n)<- mss ]
    , [ propertyToQuery ts' --> doTile        | ts'  <- ts  ]
    , [ propertyToQuery fs' --> doFloat       | fs'  <- fs  ]
    , [ propertyToQuery cfs'--> doCenterFloat | cfs' <- cfs ]
    , [ propertyToQuery ffs'--> doMyFFloat    | ffs' <- ffs ]
    , [ propertyToQuery is' --> doIgnore      | is'  <- is  ]
    , [ propertyToQuery (ClassName "VirtualBox" `And` Not (Title "Oracle VM VirtualBox Håndtering")) -->
            do t <- title
               if ("Oracle VM VirtualBox" `isSuffixOf` t)
               then do let ws = "vm-" ++ takeWhile (\c -> c /= '[') t
                       liftX $ addHiddenWorkspace ws
                       doF (W.shift ws) <+> doF (W.greedyView ws)
               else return mempty ]
    ]) <+> (composeOne . concat $
    [ [ isFullscreen -?> doFullFloat   ]
    , [ isDialog     -?> doCenterFloat ]
    , [ return True  -?> doMaster      ] -- prevent new windows from stealing focus
    ]) where
        doTile          = (ask >>= doF . W.sink)
        doMaster        = (doF W.shiftMaster)
        doMyFFloat      = (doF W.focusDown <+> doFullFloat)
        topicManageHook'= (composeAll . concat $ map (\TopicDefinition{tdName = n, tdBoundApps=as}
                        -> [ (propertyToQuery id) --> doShift n | id <- as ]) tds)

-- | A list of keybindings related to topics
-- TODO : Take as argument the other keys and make keybindings in there that overlap with topicAppsKeys
-- be bound on corresponding workspaces and the default ("", Action) be the binding in regular keys
topicEZKeys :: TopicDefinitions -> TS.TopicConfig -> [(String, X())] -> [(String, X ())]
topicEZKeys tds tc ks = ( ("M-m", bindOn topicsApps):(topicShifts ++ (shrinkAndBind [] sortedTopicAppsKeys)) )
    where
        topicsApps :: [(String, X())]
        topicsApps = map (\TopicDefinition {tdName = n, tdMenuApps = as} -> (n, spawnSelected' as)) tds

        -- | TODO : Modify to use view if tdActionOnFocus is set and possibly also to update directory
        topicShifts :: [(String, X ())]
        topicShifts = [("M-" ++ m ++ k, f i)
                        | (i, k) <- zip myNumberedTopics $ map show $ take (length myNumberedTopics) [1..]
                        , (f, m) <- [(topicWithAction,  ""), (shiftTo, "S-")]]
                      where myNumberedTopics = numberedTopics tds
                            topicWithAction  = TS.switchTopic tc
                            topicNoAction    = windows . W.view
                            shiftTo          = windows . W.shift

        -- | Shrink a list from 'sortedTopicAppsKeys' into a list of keybindings to be used in 'additionalKeysP'
        shrinkAndBind :: [(String, [(TS.Topic, X())])] -- ^ Accumulator / Already reduced list
                      -> [(String, [(TS.Topic, X())])] -- ^ Sorted list of tuples of (key string, [(Topic name, X() Action)])
                      -> [(String, X())]            -- ^ Duplicates reduced in sorted list of tuples of (key string, bindOn [(Topic name, X() Action)])
        shrinkAndBind acc []     = map (\(k, nas) -> (k, bindOn nas)) acc --base case: return key bindings
        shrinkAndBind acc (x:[]) = shrinkAndBind (x:acc) []
        shrinkAndBind acc ((k1, nas1):(k2, nas2):ks') = if k1 == k2
                                                       then shrinkAndBind ((k1,nas1++nas2):acc)            ks'
                                                       else shrinkAndBind       ((k1,nas1):acc) ((k2,nas2):ks')

        -- | Manipulate a list of 'TopicDefinition' into a sorted list of tuples with el. 1 being key and el. 2 being tuples for X.A.BindOn
        sortedTopicAppsKeys :: [( String, [(TS.Topic, X())] )]
        sortedTopicAppsKeys =
            let -- | Determines if first argument is lexigraphically smaller than the second.
                smaller :: String -> String -> Bool
                smaller s1 s2 | len1 /= len2         = (len1 < len2)
                              | otherwise            = (s1 < s2)
                    where (len1, len2) = (length s1, length s2)

                sortBinds :: (String, [(TS.Topic, X())]) -> (String, [(TS.Topic, X())]) -> Ordering
                sortBinds (k1,_) (k2,_) | k1 == k2        = EQ
                                        | k1 `smaller` k2 = LT
                                        | otherwise       = GT

                keyBindings :: [(String, [(TS.Topic, X())])]
                keyBindings = map (\(k, a) -> (k, [("", a)])) ks

                sortKeys :: [(String, [(TS.Topic, X())])] -> [(String, [(TS.Topic, X())])]
                sortKeys = sortBy sortBinds

            in sortKeys $ keyBindings ++ concat (foldr (\TopicDefinition {tdName=n, tdKeyBindings=ks'} acc
                                            -> (map (\(k,a) -> (k, [(n,a)])) ks'):acc) [] tds)





-- DEPRECATED/REPLACED/IDEAS THAT NEVER BECAME

{-

myTopicLayoutHook       = TU.topicLayoutHook myTopicDefs

-- | Transform a TopicDefinition-list into a ManageHook
myTopicLayoutHook :: ManageHook
myTopicLayoutHook = composeAll $ map (\TopicDefinition { tdName = n
                                                       , tdBoundLayout = l
                                                       , tdDir = d }
                                        -> onWorkspace n (workspaceDir d l)
                                     ) myTopicDefs

myTopicLayoutHook = avoidStruts $ renamed [CutWordsLeft 4] $ maximize $ boringWindows $ spacing 5 $
    (concat $ map (\TopicDefinition {tdName = n, tdBoundLayout = l, tdDir=d} -> onWorkspace n (workspaceDir d l)) myTopicDefs)
    myStandardLayout

-- | Transform a TopicDefinition-list into a ManageHook
topicLayoutHook :: TopicDefinitions -> ManageHook
topicLayoutHook = composeAll $ map (\TopicDefinition{tdName = n, tdBoundLayout = l, tdDir = d} -> onWorkspace n (workspaceDir d l))


gaps [(U,16), (D,16), (L,0), (R,0)]

-- |Topic key bindings to be included in master key bindings (PSP.Keybinds)
topicKeyBinds :: Ord (TopicDefinitions) => TopicDefinitions -> [(String, X())]
topicKeyBinds ts = (topicMenuKeyBind ts):rest
    where
        rest = (keyBindings [] $ sortSingleKeys ts)

myTopicKeys2 :: [(String, X())] -> [(String, X ())]
myTopicKeys2 = ( (("M-m", bindOn topicsApps):topicShifts) ++ shrinkAndBind [] sortedTopicAppsKeys )
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

keyBindings :: [(String, [(String, X())]] -> [String, X()]
keyBindings acc []   = acc
keyBindings acc x:[] = x:acc
keyBindings acc (k1, nas1):(k2, nas2):ks
    | k1 == k2       = keyBindings (k1,nas1++nas2):acc           ks
    | otherwise      = keybindings       (k1,nas1):acc (k2,nas2):ks

sortSingleKeys :: TopicDefinitions -> [( String, [(String, X())] )]
sortSingleKeys = sortKeys $ map topicToBind
    where
        sortKeys = sortBy (\x y -> compare (fst x) (fst y))
        topicToBind TopicDefinition {tdName=n,tdKeyBindings=kbs} = map toNamespaceBinding kbs
        where toNamespaceBinding :: (String, X()) -> (String, [(String, X())])
              toNamespaceBinding (k,a) = (k, [(n, a)])

--topicMenuKeyBind :: TopicDefinitions -> [(String, X ())]
topicMenuKeyBind = compileKeyBinding $ listToBind
    where compileKeyBinding :: [(String, X())] -> (String, X())
          compileKeyBinding l = (T.myTopicMenuKey, bindOn apps)

          listToBind :: TopicDefinitions -> [(String, X())]
          listToBind = foldr (\td acc -> let apps = tdMenuApps td
                                         in  if null apps
                                             then acc
                                             else (tdName td, spawnSelected' apps):acc
                             )
-}

