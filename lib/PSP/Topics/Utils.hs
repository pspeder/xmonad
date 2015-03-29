module PSP.Topics.Utils
( TopicDefinition(..)
, TopicDefinitions
, defaultTopicDefinition
, topics
, numberedTopics
, topicDirs'
, topicActions'
, topicEZKeys
, topicManageHook
) where

import XMonad
import XMonad.Layout
-- Imports we replace
import           XMonad.Actions.TopicSpace      (Dir,Topic,TopicConfig(..),switchTopic,topicAction)
-- Utilities
import qualified Data.Map as M                  (fromList,Map(..))
import           Data.List                      (sortBy)
import           Data.Char                      (isDigit)
import qualified XMonad.StackSet as W           (focusDown,greedyView,shift,shiftMaster,sink,view)
import           XMonad.Layout.PerWorkspace     (onWorkspace)
import           XMonad.Util.WindowProperties   (Property(..),propertyToQuery)
-- Hooks
import           XMonad.ManageHook
import           XMonad.Hooks.ManageHelpers
-- Actions
import           XMonad.Actions.PerWorkspaceKeys(bindOn)
-- Own modules
import           PSP.Utils                      (spawnSelected')

data TopicDefinition = TopicDefinition
    { tdName           :: !Topic                -- ^ Identifier
    , tdAction         :: !(X ())               -- ^ X Action bound to topic
    , tdActionOnStartup:: !Bool                 -- ^ Run the bound action on xmonad startup
    , tdActionOnFocus  :: !Bool                 -- ^ Run the action when ws is displayed
    , tdHidden         :: !Bool                 -- ^ Superflous for now. Decide whether to hide in
                                                -- loghook etc.
    , tdDir            :: !Dir                  -- ^ Directory the spawning shell should spawn from
    , tdMenuApps       :: ![(String, String)]   -- ^ Pretty name and exec string for 2D menu
    , tdBoundApps      :: ![Property]           -- ^ XProperties to tie in ManageHook
    , tdKeyBindings    :: ![(String, X())]      -- ^ Keys that should enabled on ws list of (key, app)s
    -- Unsure of actual type below (Possibly ModifiedLayout a l)
    --, tdBoundLayout :: !(l Window)            -- ^ The layout "hook" to be used on ws
    -- Possible TODO :
    --  Add support for some of following:
    --    - Mouse bindings
    --    - Scratchpads
    }

type TopicDefinitions = [TopicDefinition]

--defaultTopicDefinition :: (Eq TopicDefinition, Ord TopicDefinition) => TopicDefinition
defaultTopicDefinition = TopicDefinition
                     { tdName           = "1:main"
                     , tdAction         = return ()
                     , tdActionOnStartup= False
                     , tdActionOnFocus  = False
                     , tdHidden         = False
                     , tdDir            = "~"
                     , tdMenuApps       = [("Terminal", "urxvtc")]
                     , tdBoundApps      = []
                     , tdKeyBindings    = []
                     --, tdBoundLayout    = myStandardLayout
                     }

-- Exported Functions
---------------------
-- | List of topic names
topics  :: TopicDefinitions -> [Topic]
topics  = map (\x -> tdName x)

numberedTopics :: TopicDefinitions -> [Topic]
numberedTopics = foldr (\TopicDefinition{tdName=t} acc -> if isDigit $ head $ t then t:acc else acc) [[]]
-- | List containing only the head of input list
--onlyNumbers :: [String] -> (TopicDefinition -> String) -> [String]
--onlyNumbers = map (\n -> head n)

-- | Map, tying together topic names with their directories
--  (for use in TopicConfig)
topicDirs' :: TopicDefinitions -> M.Map Topic Dir
topicDirs' tds = M.fromList $ map (\x -> (tdName x, tdDir x)) tds

-- | Map from topic name to topic action
--   (for use in TopicConfig)
topicActions' :: TopicDefinitions -> M.Map Topic (X())
topicActions' tds = M.fromList $ map (\x -> (tdName x, tdAction x)) tds

-- |Transforms TopicDefinitions into a list containing only the
-- topics starting with a number ranging 1-9
--topicsWithNumber :: TopicDefinitions -> (TopicDefinition -> String -> String) -> [String] -> [TopicDefinitions] -> [String]
--topicsWithNumber = foldr (\TopicDefinition {tdName = n} acc -> if head n `elem` [1..9]
--                                                                  then n:acc
--                                                                  else acc) []



-- | ManageHook -- compile a manage hook from the following:
topicManageHook :: TopicDefinitions    -- ^ tds : TopicDefinitions to bind appropriate apps
             -> [Property]          -- ^ ts  : list of window properties to be forced into tiling
             -> [Property]          -- ^ fs  : list of window properties to float by standard floating algorithm
             -> [Property]          -- ^ cfs : list of window properties to force into centered floats
             -> [Property]          -- ^ ffs : list of window properties to force into full floats
             -> [Property]          -- ^ is  : ignored window properties
             -> [(Property, Topic)] -- ^ mss : manual shifts [(property, workspace)]
             -> ManageHook          -- ^ The ManageHook to keep track of topics, floats and fullscreen apps.
topicManageHook tds ts fs cfs ffs is mss =
    topicManageHook' <+> (composeAll . concat $
    [ [ propertyToQuery p   --> doShift n     | (p,n)<- mss ]
    , [ propertyToQuery ts' --> doTile        | ts'  <- ts  ]
    , [ propertyToQuery fs' --> doFloat       | fs'  <- fs  ]
    , [ propertyToQuery cfs'--> doCenterFloat | cfs' <- cfs ]
    , [ propertyToQuery ffs'--> doMyFFloat    | ffs' <- ffs ]
    , [ propertyToQuery is' --> doIgnore      | is'  <- is  ]
--      Previous attempts/Alternative method, that didn't quite work for my needs.
--      [ matchAny ts'  --> doTile        | ts'  <- ts  ]
--    , [ matchAny fs'  --> doFloat       | fs'  <- fs  ]
--    , [ matchAny cfs' --> doCenterFloat | cfs' <- cfs ]
--    , [ matchAny ffs' --> doMyFFloat    | ffs' <- ffs ]
--    , [ matchAny is'  --> doIgnore      | is'  <- is  ]
    ]) <+> (composeOne . concat $
    [ [ isFullscreen -?> doFullFloat   ]
    , [ isDialog     -?> doCenterFloat ]
    , [ return True  -?> doMaster      ] -- prevent new windows from stealing focus
    ]) where
        doTile          = (ask >>= doF . W.sink)
        wmrole          = stringProperty "WM_WINDOW_ROLE"
        doMaster        = (doF W.shiftMaster)
        doMyFFloat      = (doF W.focusDown <+> doFullFloat)
        topicManageHook'= (composeAll . concat $ map (\TopicDefinition{tdName = n, tdBoundApps=as}
                        -> [ (propertyToQuery id) --> doShift n | id <- as ]) tds)

-- | A list of keybindings related to topics
-- TODO : Take as argument the other keys and make keybindings in there that overlap with topicAppsKeys
-- be bound on corresponding workspaces and the default ("", Action) be the binding in regular keys
topicEZKeys :: TopicDefinitions -> TopicConfig -> [(String, X())] -> [(String, X ())]
topicEZKeys tds tc ks = ( ("M-m", bindOn topicsApps):(topicShifts ++ (shrinkAndBind [] sortedTopicAppsKeys)) )
    where
        topicsApps :: [(String, X())]
        topicsApps = map (\TopicDefinition {tdName = n, tdMenuApps = as} -> (n, spawnSelected' as)) tds

        -- | TODO : Modify to use view if tdActionOnFocus is set
        topicShifts :: [(String, X ())]
        topicShifts = [("M-" ++ m ++ k, f i)
                        | (i, k) <- zip myNumberedTopics $ map show $ take (length myNumberedTopics) [1..]
                        , (f, m) <- [(topicWithAction,  ""), (shiftTo, "S-")]]
                      where myNumberedTopics = numberedTopics tds
                            topicWithAction  = switchTopic tc
                            topicNoAction    = windows . W.view
                            shiftTo          = windows . W.shift

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
        sortedTopicAppsKeys :: [( String, [(Topic, X())] )]
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

                keyBindings :: [(String, [(Topic, X())])]
                keyBindings = map (\(k, a) -> (k, [("", a)])) ks

                sortKeys :: [(String, [(Topic, X())])] -> [(String, [(Topic, X())])]
                sortKeys = sortBy sortBinds

            in sortKeys $ concat $ foldr (\TopicDefinition {tdName=n, tdKeyBindings=ks} acc
                                            -> (map (\(k,a) -> (k, [(n,a)])) ks):acc) [] tds

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

-- | Transform a TopicDefinition-list into a ManageHook
topicLayoutHook :: TopicDefinitions -> ManageHook
topicLayoutHook = composeAll $ map (\TopicDefinition{tdName = n, tdBoundLayout = l, tdDir = d} -> onWorkspace n (workspaceDir d l))
-}
--gaps [(U,16), (D,16), (L,0), (R,0)]
{-

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
