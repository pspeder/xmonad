module PSP.Utils
( spawnShellIn
, spawnSelected'
, spawnToWorkspace
, goToSelectedWS
, shiftToSelectedWS
, withSelectedWS
, nonEmptyWS
, matchAny
, replace
) where

import XMonad
import Data.List(stripPrefix,partition)
import qualified XMonad.StackSet as W (greedyView,hidden,shift,stack,tag,visible,workspace)
import XMonad.Actions.TopicSpace (switchTopic,TopicConfig)
import XMonad.Actions.GridSelect (defaultGSConfig,gridselect,GSConfig(..))
import XMonad.Util.NamedScratchpad (namedScratchpadFilterOutWorkspace)

-- Spawn functions:
-------------------
-- | As defined in documentation for the module X.A.GridSelect
--   Spawn a shell in the given dir.
spawnShellIn :: String -> X ()
spawnShellIn dir = spawn $ "cd " ++ dir ++ " && urxvtc -e /usr/bin/zsh"

-- | From: http://ixti.net/software/2013/09/07/xmonad-action-gridselect-spawnselected-with-nice-titles.html
--   Select an application to launch from X.A.GridSelect with a pp'ed name.
spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = defaultGSConfig

-- | Start applications on specific workspaces, e.g., spawnToWorkspace "urxvt" "4"
--   http://stackoverflow.com/questions/4917820/start-applications-on-specific-workspaces-in-xmonad
spawnToWorkspace :: String -> String -> X ()
spawnToWorkspace program workspace = spawn program >> windows (W.greedyView workspace)

-- | Adapted from: http://pbrisbin.com/tags/xmonad/
--   So far unused
matchAny :: String -> Query Bool
matchAny s = liftAny (=? s) [className, title, wmname, wmrole, resource]
    where
        liftAny p = foldr ((<||>) . p) (return False)
        wmname = stringProperty "WM_NAME"
        wmrole = stringProperty "WM_WINDOW_ROLE"

-- | From: http://groups.google.com/group/fa.haskell/browse_thread/thread/daa11e5471402149?pli=1
--   Replace all occurrences of 'old' with 'new' in the list (xs@(y:ys))
--   This is especially useful in strings.
replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace _ _ []            = []
replace old new xs@(y:ys) = case stripPrefix old xs
                            of Nothing  -> y : replace old new ys
                               Just ys' -> new ++ replace old new ys'

-- | From: https://github.com/br0ns/config/blob/master/xmonad-lib/XMonad/Layout/TopicExtra.hs
goToSelectedWS :: TopicConfig -> Bool -> GSConfig WindowSpace -> X ()
goToSelectedWS topicConfig =
  withSelectedWS $ switchTopic topicConfig . W.tag

-- | From: https://github.com/br0ns/config/blob/master/xmonad-lib/XMonad/Layout/TopicExtra.hs
shiftToSelectedWS :: Bool -> GSConfig WindowSpace -> X ()
shiftToSelectedWS =
  withSelectedWS $ windows . (\ws -> W.greedyView ws . W.shift ws) . W.tag

-- | From: https://github.com/br0ns/config/blob/master/xmonad-lib/XMonad/Layout/TopicExtra.hs
withSelectedWS :: (WindowSpace -> X ()) -> Bool -> GSConfig WindowSpace -> X ()
withSelectedWS callback inclEmpty conf = do
  mbws <- gridselectWS inclEmpty conf
  case mbws of
    Just ws -> callback ws
    Nothing -> return ()

-- | From: https://github.com/br0ns/config/blob/master/xmonad-lib/XMonad/Layout/TopicExtra.hs
nonEmptyWS :: WindowSpace -> Bool
nonEmptyWS = (/= Nothing) . W.stack

-- | From: https://github.com/br0ns/config/blob/master/xmonad-lib/XMonad/Layout/TopicExtra.hs
-- Includes empty window spaces if {True}
gridselectWS :: Bool -> GSConfig WindowSpace -> X (Maybe WindowSpace)
gridselectWS inclEmpty conf =
  withWindowSet $ \ws -> do
    let hid = W.hidden ws
        vis = map W.workspace $ W.visible ws
        -- Changed from originally using Scratchpads module
        w_all = namedScratchpadFilterOutWorkspace $ hid ++ vis
        wss = if inclEmpty
              then let (nonEmp, emp) = partition nonEmptyWS w_all
                   in nonEmp ++ emp
              else Prelude.filter nonEmptyWS w_all
        ids = map W.tag wss
    gridselect conf $ zip ids wss
