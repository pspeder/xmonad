{-# OPTIONS_HADDOCK hide, prune, ignore-exports, not-home,  #-}
-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Actions.TopicDefinitions.Spawners
-- Description  : (Maybe) Useful spawner actions, which require some input
--                from 'X.A.TopicDefinitions'.
-- Copyright    : (c) Paw Saabye Pedersen, 2015
-- License      : BSD3-Style
-- Maintainer   : Paw S. Pedersen --- paw <at> pawhome <dot> net
-- Stability    : planned changes
-- Portability  :
--
-- For a short while undefined. I plan to rectify this asap.
-----------------------------------------------------------------------------
module XMonad.Actions.TopicDefinitions.ActionsAndSpawners
(-- ** Possibilities for spawning apps with the TopicDefinitions module
  tdSpawnIn
, tdSpawn
, tdSSpawnArgs
, tdSpawnTerm
, tdSpawnTo
, tdSelectSpawn
-- ** Possibilities for movement between workspaces (apart from the keybinding M-[1..9])
, tdGotoTopic
, tdSendToTopic
, tdSelectAndGotoTopic
-- ** Possibilities for doing other stuff with workspaces:
, tdSelectTopic
, tdWithSelectedTopic
, tdChangeDir
) where

import XMonad (X(..), (<+>),spawn)
import XMonad.StackSet (greedyview)
import XMonad.Actions.TopicDefinitions
import XMonad.Actions.GridSelect

-- | Spawn an application in the given directory
tdSpawnIn :: String -> String -> X()
tdSpawnIn = undefined
-- | Spawn an application in the current topic's directory.
-- Uses 'tdSpawnIn' to do its bidding.
tdSpawn :: TDConfig -> String -> X()
tdSpawn = undefined
-- | (Safe)Spawn an application whose with additional arguments in current topic's dir.
-- Uses 'tdSpawnIn' to do its bidding.
tdSSpawnArgs :: TDConfig -> String -> [String] -> X()
tdSSpawnArgs = undefined
-- | Spawn a terminal in current topic's directory.
tdSpawnTerm :: TDConfig -> X()
tdSpawnTerm = undefined
-- | Spawn an application on specified workspace (in its directory) and if
-- second argument is true, also shift to it.
tdSpawnTo :: TDConfig -> Bool -> String -> Topic -> X()
tdSpawnTerm tdc s a n = undefined
-- | Will display a 'GridSelect' menu with pretty printed names of 'X()' actions
-- to spawn from current topic's dir. Uses 'tdSpawnIn' to do its bidding.
tdSelectSpawn :: TDConfig -> [(String, String)] -> X()
tdSelectSpawn tdc as = undefined

-- | Will display a list of topics to switch to using the 'GridSelect.GSConfig'
-- specified in TDConfig.
tdSelectAndGotoTopic :: TDConfig -> X()
tdSelectTopic tdc = undefined
-- | Switch to topic and update directory. If second \"argument\" is 'True', also
-- run 'tdAction' if workspace is empty.
tdGotoTopic :: TDConfig -> Bool -> Topic -> X()
tdGotoTopic tdc a n = undefined
-- | Send currently focused window to given topic. If second argument is 'True'
-- also switch to workspace.
tdSendToTopic :: TDConfig -> Topic -> X()
tdSendToTopic tdc n = undefined

-- | Select a topic from a 'GridSelect' menu and perform some action
-- on it.
tdWithSelectedTopic :: TDConfig -> (WindowSpace -> X()) -> X()
tdWithSelectedTopic tdc f = undefined
-- | Show a 'GridSelect' menu with workspaces, using the 'GSConfig' from
-- TDConfig and \"return\" the selected workspace.
-- If second argument is true also include empty workspaces in listing.
-- If third argument is true also include hidden workspaces (including @NSP@)
-- A future version may or may not include the option of choosing a different
-- \"menu type\".
tdSelectTopic :: TDConfig -> Bool -> Bool -> X (Maybe WindowSpace)
tdSelectTopic tdc ie ih = undefined

-- | Change the directory
tdChangeDir -> TDConfig -> Dir -> X()
tdChangeDir tdc d = undefined

