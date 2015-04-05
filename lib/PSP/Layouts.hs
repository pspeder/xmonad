{-# OPTIONS_GHC -w
 -fno-warn-missing-signatures
#-}
 -- | 
module PSP.Layouts
( myTopicLayoutHook
, myStandardLayout
--, myUniLayout
, myIMLayout
, myGimpLayout
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
import XMonad.Layout.Grid (Grid(GridRatio))
-- Layouts
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

--import           PSP.Utils (matchAny)
--import           PSP.Constants (myEditor)

myTopicLayoutHook = renamed [CutWordsLeft 1] $ avoidStruts $ maximize $ boringWindows $ windowNavigation $ trackFloating
                  $ onWorkspace "1:main"  (workspaceDir "~"               mainLayout            )
                  $ onWorkspace "2:misc"  (workspaceDir "~"             $ renamed [CutWordsLeft 2] $ spacing 0 miscLayout  )
                  $ onWorkspace "3:organiser" (workspaceDir "~/mail"      organiserLayout       )
                  $ onWorkspace "4:dev"   (workspaceDir "~/dev"           layoutEditorAndPDF    )
                  $ onWorkspace "5:www"   (workspaceDir "~/downloads"     webLayout             )
                  $ onWorkspace "6:chat"  (workspaceDir "~/downloads"     chatLayout            )
                  $ onWorkspace "7:vms"   (workspaceDir "~/shares"        vmsLayout             )
                  $ onWorkspace "8:media" (workspaceDir "~/images"        mediaLayout           )
                  $ onWorkspace "remotes" (workspaceDir "~/shares"        remotesLayout         )
                  $ onWorkspace "server"  (workspaceDir "~/srv"           serverLayout          )
                  $ onWorkspace "configs" (workspaceDir "~/dev/Configs"   confLayout            )
                  myStandardLayout
                    where
                        mainLayout = myStandardLayout
                        miscLayout = myStandardLayout
                        organiserLayout = myStandardLayout
                        devLayout  = myStandardLayout
                        webLayout  = renamed [Replace"F"] $ noBorders . spacing 0 $ fullscreenFull Full
                        chatLayout = renamed [Replace"S"]
                                   $ withIM (15/100) (ClassName "Skype" `And` Role "") $ reflectHoriz
                                   $ withIM (17/100) (ClassName "Pidgin" `And` Role "buddy_list") $ reflectHoriz
                                   $ noBorders $ spacing 0 simpleTabbed
                        vmsLayout = myStandardLayout
                        mediaLayout = renamed [Replace"F"]
                                    $ withIM (18/100) (Role "gimp-dock")    $ reflectHoriz
                                    $ withIM (25/100) (Role "gimp-toolbox") $ reflectHoriz $ fullscreenFull Full
                        remotesLayout = myStandardLayout
                        serverLayout = myStandardLayout
                        confLayout = myStandardLayout


myStandardLayout = layoutTiled ||| layoutMTiled ||| layoutSTabbed ||| layoutAccordion ||| layoutFullscreenFull

--------------------------
--       LAYOUTS        --
--------------------------
layoutTiled         = renamed [Replace "V"] $ spacing 3 $ Tall 1 (2/100) (6/10)
layoutMTiled        = renamed [Replace "H"] $ spacing 3 $ Mirror layoutTiled
layoutSTabbed       = renamed [Replace "T"] $ noBorders $ spacing 0 $ simpleTabbed
layoutFullscreenFull= renamed [Replace "F"] $ noBorders $ spacing 0 $ fullscreenFull Full
layoutAccordion     = renamed [Replace "A"] Accordion
layoutDishes        = renamed [Replace "D"] $ Dishes 1 (4/6)
layoutCircle        = renamed [Replace "C"] Circle
layoutEditorAndPDF  = (combineTwoP mastering editorAndTerm pdfsAndOthers toLeftL)
                      where mastering     = TwoPane (3/100) (2/3)
                            editorAndTerm = limitWindows 2 $ spacing 0 $ layoutHints $ Mirror $ TwoPane (3/100) (85/100)
                            pdfsAndOthers = spacing 2 $ layoutDishes ||| layoutAccordion ||| layoutTiled ||| layoutMTiled
                            toLeftL       =  ClassName "Gvim"  `Or`
                                            (ClassName "URxvt" `And` Resource "editorterm")
myIMLayout = withIM (15/100) (Resource "pspeder - skype™") $ reflectHoriz $ simpleTabbed
myGimpLayout = withIM (15/100) (Resource "gimp-toolbox") $ reflectHoriz $ withIM (15/100) (Resource "gimp-dock") $ simpleTabbed


-- For windows that need be full
--myNoBordersFullLayout = noBorders $ spacing 0 $ Full
-- Rudimentary
--tiled       = renamed [Replace "tiled"]     $ Tall 1 (2/100) (6/10)
--mTiled      = renamed [Replace "mTiled"]    $ Mirror tiled
--accordion   = renamed [Replace "Accordion"] Accordion
--myTabbedLayout = renamed [Replace "pTabed"] $ addTabsAlways shrinkText defaultTheme $ Tall 1 (2/100) (6/10)
--myTabbedLayout = addTabsAlways shrinkText defaultTheme $ Tall 1 (2/100) (6/10)
--myTabbedLayout = simpleTabbed


--twoCols (n,r,right) = renamed [Replace n] $ combineTwoP (TwoPane (2/100) (65/100)) (Column 1.8 ||| Full) right (Role r)

--grid        = renamed [Replace "Grid"]      $ spacing 2 $ GridRatio (1/2)

--bothSidesMenuLayout l r = withIM (15/100) (Resource l) $ reflectHoriz $
--                          withIM (15/100) (Resource r) $ reflectHoriz $ grid

-- Complete
--uniLayout   = twoCols ("Uni Layout", "uni-gvim", rightPane)
--    where rightPane = Accordion ||| XMonad.Layout.Tabbed.tabbedAlways XMonad.Layout.Tabbed.shrinkText myTabConfig
-- myIMLayout   = bothSidesMenuLayout "buddy_list" "pspeder - skype™"

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
