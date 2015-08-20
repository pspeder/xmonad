-- {-# LANGUAGE FlexibleContexts,IncoherentInstances #-}
{-# LANGUAGE FlexibleInstances,TypeSynonymInstances #-}
module XMonad.Config.Psp.Configs
( myXPConfig
, myDarkXPConfig
, myGSConfig
, myColorizer
, progs
) where

import           XMonad-- (spawn,Window(..),X(..))
import           XMonad.StackSet (tag)
import           Control.Monad.Writer (fix)

import           Graphics.X11.Types
import           Data.List (isPrefixOf)
import qualified Data.Map as M (Map(..),fromList,mapKeys)
import           XMonad.Prompt (XPConfig(..)
                               ,defaultXPKeymap
                               ,XPPosition(..))
import XMonad.Hooks.DynamicLog
import XMonad.Actions.TopicSpace (Dir)
import XMonad.Util.WindowProperties (Property(..))
import XMonad.Util.Run (hPutStrLn)
import XMonad.Actions.GridSelect (buildDefaultGSConfig,defaultGSConfig,gridselect,GSConfig(..),colorRangeFromClassName,TwoD(..),makeXEventhandler,shadowWithKeymap,cancel,select,move,setPos,substringSearch,defaultColorizer,HasColorizer(..),stringColorizer)

import XMonad.Config.Psp.Utils (nonEmptyWS)
import XMonad.Actions.TopicDefinitions (ProgSetup(..))

instance HasColorizer WindowSpace where
  defaultColorizer ws isFg =
    if nonEmptyWS ws || isFg
    then stringColorizer (tag ws) isFg
         -- Empty workspaces get a dusty-sandy-ish colour
    else return ("#CAC3BA", "white")

myGSConfig :: HasColorizer a => GSConfig a
myGSConfig = defaultGSConfig { gs_navigate = fix $ \self ->
    let navKeyMap = M.mapKeys ((,) 0) $ M.fromList $
                [(xK_Escape, cancel)
                ,(xK_Return, select)
                ,(xK_slash , substringSearch self)]
           ++
            map (\(k,a) -> (k,a >> self))
                [(xK_Left  , move (-1,0 ))
                ,(xK_h     , move (-1,0 ))
                ,(xK_n     , move (-1,0 ))
                ,(xK_Right , move (1,0  ))
                ,(xK_l     , move (1,0  ))
                ,(xK_i     , move (1,0  ))
                ,(xK_Down  , move (0,1  ))
                ,(xK_j     , move (0,1  ))
                ,(xK_e     , move (0,1  ))
                ,(xK_Up    , move (0,-1 ))
                ,(xK_u     , move (0,-1 ))
                ,(xK_y     , move (-1,-1))
                ,(xK_m     , move (1,-1 ))
                ,(xK_space , setPos (0,0))
                ]
    in makeXEventhandler $ shadowWithKeymap navKeyMap (const self) }

myDarkXPConfig :: XPConfig
myDarkXPConfig = defaultXPConfig
    { bgColor           = "black"
    , fgColor           = "grey"
    , promptBorderWidth = 0
    , position          = Bottom
    , height            = 15
    , historySize       = 256
    }

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
    { bgColor           = "black"
    , fgColor           = "#3399ff"
    , promptBorderWidth = 0
    , position          = Top
    , height            = 15
    , historySize       = 256
    }

defaultXPConfig =
    XPC { font              = "-misc-fixed-*-*-*-*-12-*-*-*-*-*-*-*"
        , bgColor           = "grey22"
        , fgColor           = "grey80"
        , fgHLight          = "black"
        , bgHLight          = "grey"
        , borderColor       = "white"
        , promptBorderWidth = 1
        , promptKeymap      = defaultXPKeymap
        , completionKey     = xK_Tab
        , changeModeKey     = xK_grave
        , position          = Bottom
        , height            = 18
        , historySize       = 256
        , historyFilter     = id
        , defaultText       = []
        , autoComplete      = Nothing
        , showCompletionOnTab = False
        , searchPredicate   = isPrefixOf
        , alwaysHighlight   = False
        }

-- GridSelect color scheme
myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
    (0x00,0x00,0x00) --lowest inactive bg
    (0xFF,0x8F,0xFF) --highest inactive bg
    (0x58,0x71,0xFF) --active bg
    (0xBB,0xBB,0xBB) --inactive fg
    (0x00,0x00,0x00) --active fg

myGSConfig' :: GSConfig Window
myGSConfig' = (buildDefaultGSConfig myColorizer)
    { gs_cellheight  = 50
    , gs_cellwidth   = 200
    , gs_cellpadding = 10
    , gs_font        = dzenFont
    , gs_navigate    = myNavigation
    }

myNavigation :: TwoD a (Maybe a)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
  where navKeyMap = M.fromList
            [ ((0,xK_Escape), cancel)
            , ((0,xK_Return), select)
            , ((0,xK_space) , substringSearch myNavigation)
            , ((0,xK_Left)  , move (-1,0)  >> myNavigation)
            , ((0,xK_h)     , move (-1,0)  >> myNavigation)
            , ((0,xK_Right) , move (1,0)   >> myNavigation)
            , ((0,xK_l)     , move (1,0)   >> myNavigation)
            , ((0,xK_Down)  , move (0,1)   >> myNavigation)
            , ((0,xK_j)     , move (0,1)   >> myNavigation)
            , ((0,xK_Up)    , move (0,-1)  >> myNavigation)
            , ((0,xK_k)     , move (0,-1)  >> myNavigation)
            ]
        -- The navigation handler ignores unknown key symbols
        navDefaultHandler = const myNavigation

dzenFont = "-*-montecarlo-medium-r-normal-*-11-*-*-*-*-*-*-*"

{-
myDefaultTabbedConfig :: Theme
myDefaultTabbedConfig = defaultTheme
    { activeColor           = C.darkBlue
    , inactiveColor         = C.darkGrey
    , urgentColor           = C.lightBlue
    , activeBorderColor     = C.darkBlack
    , inactiveBorderColor   = C.darkWhite
    , urgentBorderColor     = C.darkBlack
    , activeTextColor       = C.
    , inactiveTextColor     =
    , urgentTextColor       =
    , fontName              =
    , decoWidth             =
    , decoHeight            =
    , windowTitleAddons     =
    , windowTitleIcons      =
    }
-}

progs :: ProgSetup
progs = ProgSetup
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

