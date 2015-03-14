module PSP.Topics.Utils
( TopicDefinition
, TopicDefinitions
, defaultTopicDefinition
, topics
, topicDirs
, topicActions
, topicKeyBinds
, topicManageHook
, topicLayoutHook
, XMonad.Actions.PerWorkspaceKeys (bindOn)
) where

import XMonad
import XMonad.Layout
-- Utilities
import qualified Data.Map as M
import           XMonad.StackSet                (RationalRect)
import           XMonad.Layout.PerWorkspace  (onWorkspace)
-- Hooks
import           XMonad.ManageHook
import           XMonad.Hooks.ManageHelpers
-- Actions
import           XMonad.Actions.TopicSpace      (Dir)
import           XMonad.Actions.PerWorkspaceKeys (bindOn)
import qualified PSP.Topics (myTopicMenuKey,myTopics)
import           PSP.Layouts (myStandardLayout)
--import           PSP.Constants (myTerminal)

data TopicDefinition = TopicDefinition
                     { tdName        :: String
                     , tdAction      :: X ()
                     , tdAtStartup   :: Bool
                     , tdHidden      :: Bool
                     , tdMenuApps    :: [(String, X())] -- Pretty name and exec for 2D menu
                     , tdBoundApps   :: [String]        -- Some window property for ManageHook
                     , tdKeyBindings :: [(String, X())] -- [(key, app)]
                     , tdBoundFolder :: String
                     , tdBoundLayout :: ManageHook
                     }

type TopicDefinitions = [TopicDefinition]

--defaultTopicDefinition :: (Eq TopicDefinition, Ord TopicDefinition) => TopicDefinition
defaulTopicDefinition = TopicDefinition
                     { tdName         = "1:main"
                     , tdAction       = return ()
                     , tdAtStartup    = False
                     , tdHidden       = False
                     , tdMenuApps     = [("Terminal", spawn "urxvtc")]
                     , tdBoundFolder  = "~"
                     , tdBoundApps    = []
                     , tdBoundKeys    = []
                     , tdBoundLayout  = myStandardLayout
                     }

tdDefaultLayout :: ManageHook
tdDefaultLayout = tiled ||| mirrored ||| Full
    where
      mirrored = Mirror tiled
      tiled    = Tall nmaster delta ratio
      nmaster = 1       -- no. of windows in master pane
      delta   = 2/100   -- %-age of screen to resize by
      ratio   = 6/10    -- %-age of screen for master pane

-- Exported Functions
---------------------

-- |List of topic names
topics  :: TopicDefinitions -> [String]
topics  = map (\x -> tdName x)

-- |Map, tying together topic names with their directories
--  (for use in TopicConfig)
topicDirs :: TopicDefinitions -> Map (String,Dir)
topicDirs = M.fromList $ map (\x -> (tdName x, tdDir x))


-- |Topic key bindings to be included in master key bindings (PSP.Keybinds)
topicKeyBinds :: Ord (TopicDefinitions) => TopicDefinitions -> [String, X()]
topicKeyBinds ts = (topicMenuKeyBind ts):rest
    where
        rest = (keyBindings [] $ sortSingleKeys ts)

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

-- | Transform a TopicDefinition-list into a ManageHook
topicLayoutHook :: TopicDefinitions -> ManageHook
topicLayoutHook = composeAll $ map (\TopicDefinition{tdName = n, tdBoundLayout = l, tdDir = d} -> onWorkspace n (workspaceDir d l))

-- |Transforms TopicDefinitions into a list containing only the
-- topics starting with a number ranging 1-9
topicsWithNumber :: TopicDefinitions -> (TopicDefinition -> String -> String) -> [String] -> [TopicDefinitions] -> [String]
topicsWithNumber = foldr (\TopicDefinition {tdName = n} acc -> if head n `elem` [1..9]
                                                                  then n:acc
                                                                  else acc) []
-- |List containing only the head of input list
onlyNumbers :: [String] -> (TopicDefinition -> String) -> [String]
onlyNumbers = map (\n -> head n)

-- |ManageHook -- compile a manage hook from the following:
myManageHook :: String -- ^ floats
             -> String -- ^ centered floats
             -> String -- ^ full floats
             -> String -- ^ ignored window properties
             -> TopicDefinitions -- ^ TopicDefinitions list
             -> ManageHook
myManageHook fs cfs ffs is ts =
    composeAll (concat
    [ [ matchAny fs'    --> doFloat         | fs'   <- fs  ]
    , [ matchAny cfs'   --> doCenterFloat   | cfs'  <- cfs ]
    , [ matchAny ffs'   --> goodFullFloat   | ffs'  <- ffs ] ]
    , [ matchAny is'    --> doIgnore        | is'   <- is  ] ])
    <+>
    [ isDialog          -?> doFloat
    , isFullscreen      -?> goodFullFloat ]
    <+> topicManageHook ts []
    where
        myFullFloat = (doF W.focusDown <+> doFullFloat)

topicManageHook (TopicDefinition {tdName = n, tdBoundApps = as}:ts) acc
    | null as   = topicManageHook ts acc
    | null ts   = newAcc
    | otherwise = topicManageHook ts newAcc
    where
        newAcc  = [matchAny id --> doShift n | id <- as] ++ acc

-- | From: http://ixti.net/software/2013/09/07/xmonad-action-gridselect-spawnselected-with-nice-titles.html
-- Select an application to launch from X.A.GridSelect with a pp'ed name.
spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = gsConfig
