{-# OPTIONS_GHC -w
 -fno-warn-missing-signatures
#-}
module PSP.Layouts
( myStandardLayout
--, myUniLayout
, myIMLayout
, myGimpLayout
, ToggleLayouts(..)
)
where

import XMonad
import XMonad.Layout

-- Utilities
import           XMonad.Layout.WindowNavigation                         -- Allow navigation and movement of windows vim-style
import           XMonad.Layout.Spacing                                  -- Put spacing around windows
import           XMonad.Layout.PerWorkspace                             -- Individual layouts
import           XMonad.Layout.Maximize                                 -- Temporarily Maximize Window
import           XMonad.Layout.WorkspaceDir                             -- Assign local dir to workspace
import           XMonad.Layout.Reflect (reflectHoriz)                   -- Allow "turning around" ws layout
import           XMonad.Layout.ComboP                                   -- 
import           XMonad.Layout.Grid (Grid(GridRatio))
import           XMonad.Layout.Renamed
import           XMonad.Layout.ToggleLayouts

-- Layouts
import           XMonad.Layout.Accordion                                -- Stack windows like an accordion
import           XMonad.Layout.IM                                       -- For multi-window apps (Gimp/Pidgin)
import           XMonad.Layout.NoBorders                                -- Some windows should be plain
import           XMonad.Layout.Tabbed -- (tabbedAlways,shrinkText,defaultTheme) -- Tabbed layout
import           XMonad.Layout.TwoPane                                  -- -- || --
import           XMonad.Layout.Column                                   -- A single column

--import           PSP.Utils (matchAny)
--import           PSP.Constants (myEditor)

myStandardLayout = tiled ||| mTiled ||| accordion ||| myNoBordersFullLayout

-- For windows that need be full
myNoBordersFullLayout = noBorders $ spacing 0 $ Full
-- Rudimentary
tiled       = renamed [Replace "tiled"]     $ Tall 1 (2/100) (6/10)
mTiled      = renamed [Replace "mTiled"]    $ Mirror tiled
accordion   = renamed [Replace "Accordion"] Accordion
--myTabbedLayout = renamed [Replace "pTabed"] $ addTabsAlways shrinkText defaultTheme $ Tall 1 (2/100) (6/10)
--myTabbedLayout = addTabsAlways shrinkText defaultTheme $ Tall 1 (2/100) (6/10)
myTabbedLayout = simpleTabbed


twoCols (n,r,right) = renamed [Replace n] $ combineTwoP (TwoPane (2/100) (65/100)) (Column 1.8 ||| Full) right (Role r)

grid        = renamed [Replace "Grid"]      $ spacing 2 $ GridRatio (1/2)

--bothSidesMenuLayout l r = withIM (15/100) (Resource l) $ reflectHoriz $
--                          withIM (15/100) (Resource r) $ reflectHoriz $ grid

-- Complete
--uniLayout   = twoCols ("Uni Layout", "uni-gvim", rightPane)
--    where rightPane = Accordion ||| XMonad.Layout.Tabbed.tabbedAlways XMonad.Layout.Tabbed.shrinkText myTabConfig

--myIMLayout   = bothSidesMenuLayout "buddy_list" "pspeder - skype™"
myIMLayout = withIM (15/100) (Resource "pspeder - skype™") $ reflectHoriz $ simpleTabbed

-- myGimpLayout = bothSidesMenuLayout "gimp-toolbox" "gimp-dock"
myGimpLayout = withIM (15/100) (Resource "gimp-toolbox") $ reflectHoriz $ withIM (15/100) (Resource "gimp-dock") $ simpleTabbed

-- For windows that need be full
myNoBordersFullLayout = noBorders $ spacing 0 $ Full

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
