module PSP.Configs
( myXPConfig
, myGSConfig
, myColorizer
) where

xpc :: XPConfig
xpc = defaultXPConfig { bgColor  = "black"
                      , fgColor  = "grey"
                      , promptBorderWidth = 0
                      , position = Bottom
                      , height   = 15
                      , historySize = 256
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

-- GridSelect theme
--myGSConfig :: t -> GSConfig Window
myGSConfig = (buildDefaultGSConfig myColorizer)
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

myTopicConfig :: TopicConfig
myTopicConfig = defaultTopicConfig
  { topicDirs           = myTopicDirs
  , defaultTopicAction  = const $ return ()
  , defaultTopic        = myDefaultTopic
  , topicActions        = myTopicActions
  }

-- Loghook
myLogHook h = dynamicLogWithPP $ defaultPP
    -- display current workspace as darkgrey on light grey (opposite of 
    -- default colors)
    { ppCurrent         = dzenColor "#303030" "#909090" . pad 
    -- display other workspaces which contain windows as a brighter grey
    , ppHidden          = dzenColor "#909090" "" . pad 
    -- display other workspaces with no windows as a normal grey
    , ppHiddenNoWindows = dzenColor "#606060" "" . pad 
    -- display the current layout as a brighter grey
    , ppLayout          = dzenColor "#909090" "" . pad 
    -- if a window on a hidden workspace needs my attention, color it so
    , ppUrgent          = dzenColor "#ff0000" "#803333" . pad . dzenStrip
    -- shorten if it goes over 100 characters
    , ppTitle           = shorten 100
    -- no separator between workspaces
    , ppWsSep           = ""
    -- put a few spaces between each object
    , ppSep             = " "
    -- output to the handle we were given as an argument
    , ppOutput          = hPutStrLn h
    }

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

pspDefaults :: ProgSetup
pspDefaults = ProgSetup
    { pspTerminal       = "urxvtcd"
    , pspPDF            = "xournal"
    , pspMail           = "mutt"
    , pspEditor         = "gvim"
    , pspBrowser        = "firefox "
    , pspNewBrowserTab  = "firefox -new-tab "
    , pspNewBrowserWin  = "firefox -new-window "
    , pspPrivBrowserWin = "firefox -private-window "
    , pspFloats         = ["xclock"]
    , pspCFloats        = ["SMPlayer","MPlayer","VirtualBox","XMessage"
                          ,"XFontSel","bashrun","zshrun"
                          ,"Google Chrome Options","Chromium Options"]
    , pspFullFloats     = ["XBMC","Kodi"]
    , pspIgnores        = ["desktop","desktop_window","notify_osd"
                          ,"staleontray","trayer","dzen2","dzen2-bar"]
    }
