module CustomUrgencyHook
( LibNotifyUrgencyHook(..)
, withUrgencyHookC
, withUrgencyHook
, UrgencyConfig(..)
, urgencyConfig
, RemindWhen(..)
, SuppressWhen(..)
) where

import XMonad (whenJust,windowset,gets)
import XMonad.Hooks.UrgencyHook(RemindWhen(..),SuppressWhen(..),UrgencyHook(..),UrgencyConfig(..),urgencyConfig,withUrgencyHook,withUrgencyHookC)
import XMonad.Util.NamedWindows(getName)
import XMonad.Util.Run(safeSpawn)
import qualified XMonad.StackSet as W

-- | From: https://github.com/br0ns/config/blob/master/xmonad-lib/XMonad/Hooks/UrgencyExtra.hs
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name <- getName w
    ws <- gets windowset
    whenJust (W.findTag w ws) (flash name)
      where flash name index =
              --if index == "im"
              --then spawn "~/.xmonad/blink"
              --else
                safeSpawn "notify-send" [index ++ ": " ++ show name]
