-- | From: https://github.com/br0ns/config/blob/master/xmonad-lib/XMonad/Hooks/UrgencyExtra.hs
module XMonad.Hooks.LibNotifyUrgency (LibNotifyUrgencyHook(..)) where

import XMonad (whenJust,windowset,gets)
import XMonad.Hooks.UrgencyHook(UrgencyHook(..))
import XMonad.Util.NamedWindows(getName)
import XMonad.Util.Run(safeSpawn)
import qualified XMonad.StackSet as W (findTag)

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

