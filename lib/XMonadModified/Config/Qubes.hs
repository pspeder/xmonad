module XMonadModified.Config.Qubes where

import System.Exit
import Graphics.X11.Xlib.Cursor                     ( xC_left_ptr )
import Graphics.X11.Types                           ( mod4Mask )
import Data.Monoid                                  ( mconcat )

import XMonad.Config.Kde                            ( kde4Config )

import           XMonad.Util.WindowProperties       ( Property(..) )        -- Classes to handle window properties
import           XMonad.Util.Cursor                 ( setDefaultCursor )    -- Does what function name suggests
import           XMonad.Hooks.SetWMName             ( setWMName )           -- From documentation: "May be useful for making Java GUI programs work..."
import           XMonad.Hooks.EwmhDesktops          ( ewmh                  -- Make XMonad compliant with ewmh desktop specification
                                                    , ewmhDesktopsEventHook
                                                    , ewmhDesktopsLogHook
                                                    , ewmhDesktopsStartup )
import           XMonad.Hooks.ManageDocks           ( manageDocks )         -- Regard dock-type applications' properties
import           XMonad.Actions.UpdateFocus         ( adjustEventInput      -- Adjusts event mask to pick up pointer movements
                                                    , focusOnMouseMove )    -- Event hook handle for updating focus on mouse move
import           XMonad.Actions.TopicSpace          ( (>*>) )
import           XMonad.Actions.RotSlaves           ( rotSlavesUp,rotSlavesDown )

import           XMonad.Hooks.UrgencyHook           ( withUrgencyHookC, withUrgencyHook
                                                    , UrgencyConfig(..), urgencyConfig
                                                    , SuppressWhen(..), RemindWhen(..)
                                                    , NoUrgencyHook(..) )
import           XMonad.Layout.Fullscreen           ( fullscreenEventHook, fullscreenManageHookWith )
import           XMonad.Hooks.LibNotifyUrgency      ( LibNotifyUrgencyHook(..) )
import           XMonad.Hooks.EventHooks            ( urxvtEventHook )

qubesConfig qc =
    withUrgencyHookC LibNotifyUrgencyHook
                     urgencyConfig { suppressWhen = OnScreen
                                   , remindWhen   = Repeatedly 5 90 }
  . withUrgencyHook  NoUrgencyHook
  . ewmh
  $ kde4Config
    { modMask           = mod4Mask
    , terminal          = qcTerm qc
    , workspaces        = topics $ qcTopicDefs qc
    , startupHook       = ewmhDesktopsStartup           <+>
                          setDefaultCursor xC_left_ptr  <+>
                          setWMName "LG3D"              <+>
                          adjustEventInput              <+>
                          topicStartupHook pspTDConfig pspTopicDefs pspProgs
    , handleEventHook   = ewmhDesktopsEventHook         <+>
                          focusOnMouseMove              <+>
                          fullscreenEventHook           <+>
                          urxvtEventHook
    , manageHook        = manageHook kde4Config         <+>
                          dynamicMasterHook             <+>
                          insertPosition Below Newer    <+>
                          manageDocks                   <+>
                          pspNamedScratchpadManageHook  <+>
                          topicManageHook pspTopicDefs
                                          [(ClassName "Qubes-manager")]     -- ts
                                          []                       -- fs
                                          [(ClassName "Skype" `And`
                                            Title "Options")]      -- cfs
                                          []                       -- ffs
                                          []                       -- is
                                          []                       -- mss
    , layoutHook        = myTopicLayoutHook
    }

type QDomain = String
data QubesAction = QX (QDomain, X())
                 | QS (QDomain, String)
                 | QA (QDomain, QAction)

data QAction = Open
             | OpenInDVM
             | Open

data DisplayMode = VM_WS | TOPIC_WS

class () => where
    

data QConfig = Qconfig
    { qcTerm            :: String
    , qcFloats          :: [(Domain, WindowProperties)]
    , qcCFloats         :: [(Domain, WindowProperties)]
    , qcFullFloats      :: [(Domain, WindowProperties)]
    , qcIgnores         :: [(Domain, WindowProperties)]
    , qcTopicDefs       :: [QubesTopicDefinition]
    , tdcDefaultTopic   :: TS.Topic
    , tdcDefaultDir     :: TS.Dir
    , tdcDefultAction   :: (TS.Topic -> X())
    , tdcMaxHistory     :: Int
    , tdcMenuKey        :: Key
    , tdcStartupApps    :: [X()]
    }

defaultQConfig =
    { qcTerm        = "konsole"
    , qcFloats      = [ Resource "xclock" ]
    , qcCFloats     = [ ClassName "SMPlayer", ClassName "MPlayer", ClassName"XMessage"
                      , ClassName "XFontSel", ClassName "bashrun", ClassName "zshrun"
                      , Title "Google Chrome Options", Title "Chromium Options"]
    , qcFullFloats  = [ ClassName "XBMC", ClassName "Kodi"]
    , qcIgnores     = [ Resource "desktop",Resource "desktop_window",Resource "notify_osd", ClassName "Dunst"
                      , Resource "staleontray", Resource "trayer",Resource "dzen2", Resource "dzen2-bar"]
    , qcStartup     = []--[spawn "pidgin"], safeSpawn "apulse32" ["skype"]]
    , qcTopicDefs   = []
    }

