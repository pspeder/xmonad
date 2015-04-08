-- | A Collection of event hooks
module XMonad.Hooks.EventHooks where

import XMonad (Event(..))
import Control.Monad.Writer (All(..))
import XMonad.Layout.LayoutHints (hintsEventHook)
import XMonad.Util.WindowProperties (hasProperty,Property(ClassName))

-- | Don't hint URxvt windows
-- from:
urxvtEventHook e = case e of
    PropertyEvent { ev_window = w } -> do isURXVT <- hasProperty (ClassName "URxvt") w
                                          if not isURXVT then hintsEventHook e
                                                         else return (All True)
    _                               -> return (All True)
