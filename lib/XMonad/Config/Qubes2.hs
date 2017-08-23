module Qubes where

import XMonad
import XMonad.Layout
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName

qubesConfig c =
    withUrgencyHookC LibNotifyUrgencyHook
                    urgencyConfig { suppressWhen = OnScreen
                                  , remindWhen = Repeatedly 5 90 }
    . withUrgencyHook NoUrgencyHook
    . ewmh
    $ def
    { terminal          = "konsole"
    , startupHook       = ewmhDesktopsStartup <+>
                          setWMName "LG3D" <+>
                          adjustEventInput
    , handleEventHook   = ewmhDesktopsEventHook <+>
                          fullscreenEventHook
    , manageHook        = manageHook defaultConfig <+>
                          dynamicMasterHook <+>
                          insertPosition Below Never <+>
                          manageDocks
    , logHook           = logHook def >>
                          ewmhDesktopsLogHook
    }
