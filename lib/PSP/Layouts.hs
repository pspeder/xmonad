{-# LANGUAGE TypeSynonymInstances,MultiParamTypeClasses,DeriveDataTypeable #-}
{-# OPTIONS_GHC -w
 -fno-warn-missing-signatures
#-}
 -- | 
module PSP.Layouts
( myTopicLayoutHook
, myStandardLayout
, ToggleLayouts(..)
)
where

import XMonad
import XMonad.Layout
-- Utilities
import XMonad.Hooks.ManageDocks (avoidStruts, ToggleStruts(..))
import XMonad.Layout.BoringWindows (boringWindows)
import XMonad.Layout.LayoutHints
import XMonad.Layout.LimitWindows
import XMonad.Layout.PerWorkspace                             -- Individual layouts
import XMonad.Layout.TrackFloating
import XMonad.Layout.WindowNavigation                         -- Allow navigation and movement of windows vim-style
import XMonad.Layout.Spacing                                  -- Put spacing around windows
import XMonad.Layout.Maximize                                 -- Temporarily Maximize Window
import XMonad.Layout.WorkspaceDir                             -- Assign local dir to workspace
import XMonad.Layout.Reflect (reflectHoriz,reflectVert)       -- Allow "turning around" ws layout
import XMonad.Layout.Renamed
import XMonad.Layout.ToggleLayouts
import XMonad.Util.Types (Direction2D(..))
-- Layouts
import XMonad.Layout.Grid (Grid(GridRatio))
import XMonad.Layout.Cross
import XMonad.Layout.Drawer
import XMonad.Layout.Simplest
import XMonad.Layout.Column
import XMonad.Layout.FixedColumn
import XMonad.Layout.Circle
import XMonad.Layout.Fullscreen (fullscreenFull,FullscreenFull(..))
import XMonad.Layout.StackTile
import XMonad.Layout.Dishes
import XMonad.Layout.TwoPane
import XMonad.Layout.ComboP (combineTwoP,CombineTwoP(..),SwapWindow)
import XMonad.Layout.Accordion                                -- Stack windows like an accordion
import XMonad.Layout.IM                                       -- For multi-window apps (Gimp/Pidgin)
import XMonad.Layout.NoBorders                                -- Some windows should be plain
import XMonad.Layout.Tabbed -- (tabbedAlways,shrinkText,defaultTheme) -- Tabbed layout
import XMonad.Layout.TwoPane                                  -- -- || --
import XMonad.Layout.Column                                   -- A single column
import qualified XMonad.Util.WindowProperties as WP

--------------------------
--    MAIN LAYOUTHOOK   --
--------------------------
myTopicLayoutHook =
    windowNavigation . avoidStruts . trackFloating . boringWindows . renamed [CutWordsLeft 1] . maximize $
    onWorkspace "1:main" (workspaceDir "~"              $ myStandardLayout  )$
    onWorkspace "2:misc" (workspaceDir "~"              $ myStandardLayout  )$
    onWorkspace "3:organiser" (workspaceDir "~/mail"    $ mailL             )$
    onWorkspace "4:dev" (workspaceDir "~/dev"           $ devL              )$
    onWorkspace "5:www" (workspaceDir "~/downloads"     $ webL              )$
    onWorkspace "6:chat" (workspaceDir "~/downloads"    $ chatL             )$
    onWorkspace "7:vms" (workspaceDir "~/shares"        $ myStandardLayout  )$
    onWorkspace "8:media" (workspaceDir "~/images"      $ mediaL            )$
    onWorkspace "remotes" (workspaceDir "~/shares"      $ myStandardLayout  )$
    onWorkspace "server" (workspaceDir "~/srv"          $ myStandardLayout  )$
    onWorkspace "configs" (workspaceDir "~/dev/Configs" $ myStandardLayout  )$
    myStandardLayout
    where mailL  = layoutFullscreenFull ||| withSpacing layoutMTallD
          devL   = let masterL = TwoPane resizePercentage (77/100)
                       editorL = layoutStackTile 1 (9/10)
                       comboL l= combineTwoP masterL editorL l props
                       pdfsL   = myStandardLayout
                       codeL   = layoutStackTileD
                       props   = ClassName "Gvim"  `Or` (ClassName "URxvt"
                                                   `And` Resource "editorterm")

                       pdfEditor = renamed [ CutWordsLeft 1, CutWordsRight 11
                                           , Prepend "p(", Append ")"] $ comboL pdfsL
                       codeEditor= renamed [ CutWordsLeft 1, CutWordsRight 11
                                           , Prepend "c(", Append ")"] $ comboL codeL
                   in codeEditor ||| pdfEditor
          webL   = withSpacing (layoutCircle ||| layoutGrid (4/3) ||| layoutCrossD)
               ||| layoutFullscreenFull
          chatL  = renamed [CutWordsLeft 2]
                 $ withIM (15/100) (ClassName "Skype" `And` Resource "pspeder - skype™")
                 $ reflectHoriz
                 $ renamed [CutWordsLeft 2]
                 $ withIM (17/100) (ClassName "Pidgin" `And` Role "buddy_list")
                 $ reflectHoriz
                 $ layoutGridD
          mediaL = renamed [CutWordsLeft 2]
                 $ withIM (19/100) (ClassName "gimp-dock") $ reflectHoriz
                 $ renamed [CutWordsLeft 2]
                 $ withIM (25/100) (Role "gimp-toolbox")   $ reflectHoriz
                 $ layoutGridD

myStandardLayout = withSpacing (layoutTallD ||| layoutMTallD) ||| layoutFullscreenFull

resizePercentage = (2/100)
withSpacing      = lAddSpacing layoutSpacing
layoutSpacing    = 2
layoutMasterSize = (6/10)

--------------------------
-- LAYOUTS W. DEFAULTS  --
--------------------------
layoutTallD             = layoutTall 1 layoutMasterSize
layoutMTallD            = layoutMTall 1 layoutMasterSize
layoutStackTileD        = layoutStackTile 1 (6/10)
layoutCrossD            = layoutCross (5/6)
layoutGridD             = layoutGrid (1/2)
layoutAccordionD        = layoutAccordion
layoutDishesD           = layoutDishes 1 layoutMasterSize

--------------------------
--   (NAMED) LAYOUTS    --
--------------------------
layoutFullscreenFull= renamed [Replace "F"] $ noBorders $ fullscreenFull Full
layoutSTabbedD      = renamed [Replace "t"] $ noBorders $ simpleTabbed
layoutCircle        = renamed [Replace "C"] $ Circle

layoutTall m p      = renamed [Replace "H"] $ Tall m resizePercentage p
layoutMTall m p     = renamed [Replace "V"] $ Mirror $ Tall m resizePercentage p
layoutStackTile m p = renamed [Replace "S"] $ StackTile m resizePercentage p
layoutGrid r        = renamed [Replace "G"] $ GridRatio r
layoutCross p       = renamed [Replace "x"] $ Cross p resizePercentage
layoutAccordion     = renamed [Replace "A"] $ Accordion
layoutDishes m p    = renamed [Replace "D"] $ Dishes m p

--------------------------
--  LAYOUT COMBINATORS  --
--------------------------
lAddSpacing s l = renamed [CutWordsLeft 2] $ spacing s $ l
--addedTabs s t l         = renamed [CutWordsLeft 2, Prepend "T(", Append ")"] $ spacing s $ addTabsAlways shrinkText t l
--bothSidesMenuLayout (lProp,lPerc) (rProp,rPerc) layout =
--    renamed [Prepend "I(",Append ")"] $ wim (lProp,lPerc)
--                                      $ wim (rProp,rPerc) $ layout
--    where wim (pr,pe) l = renamed [CutWordsLeft 2] $ withIM (pe/100) pr $ reflectHoriz $ l

--layoutDrawerD d p   = layoutDrawerT d (0,(1/3)) p (Tall 1 (2/100) (6/10)) layoutTiled
--layoutDrawerT d (sc,so) p dl l = case d of L -> draw `onLeft` l
--                                           D -> draw `onBottom` l
--                                           U -> draw `onTop` l
--                                           R -> draw `onRight` l
--                                 where draw = drawer sc so p dl
--twoCols (n,r,right) = renamed [Replace n] $ combineTwoP (TwoPane (2/100) (65/100)) (Column 1.8 ||| Full) right (Role r)
--uniLayout   = twoCols ("Uni Layout", "uni-gvim", rightPane)
--    where rightPane = Accordion ||| XMonad.Layout.Tabbed.tabbedAlways XMonad.Layout.Tabbed.shrinkText myTabConfig

--------------------------
--        MISC          --
--------------------------
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
