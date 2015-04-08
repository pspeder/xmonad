{-# LANGUAGE TypeSynonymInstances
           , MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts #-}
{-# OPTIONS_GHC -w -fno-warn-missing-signatures #-}
module XMonad.Config.Psp.Layouts
( myTopicLayoutHook
, myStandardLayout
, ToggleLayouts(..)
)
where

import XMonad hiding ((|||))
import XMonad.Layout hiding ((|||))
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LayoutCombinators
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
import XMonad.Layout.CenteredMaster
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

--import qualified PSP.Topics.Utils as TU

--------------------------
--    MAIN LAYOUTHOOK   --
--------------------------
myTopicLayoutHook =
    windowNavigation . avoidStruts . trackFloating . boringWindows . renamed [CutWordsLeft 1] . maximize .
    set ("1:main"     , "~"             , topRightMaster myStandardLayout  ) .
    set ("2:misc"     , "~"             , myStandardLayout  ) .
    set ("3:organiser", "~/mail"        , layoutFullscreenFull
                                      ||| layoutMTallD      ) .
    set ("4:dev"      , "~/dev"         ,
         let masterL = TwoPane resizePercentage (77/100)
             editorL = renamed [CutWordsLeft 1]
                     . layoutHintsWithPlacement (0.5, 0.5)
                     . noBorders
                     $ layoutStackTile 1 (9/10)
             comboL l= combineTwoP masterL editorL l props
             pdfsL   = myStandardLayout
             codeL   = layoutMTallD ||| layoutSTabbedD
             props   = ClassName "Gvim"  `Or` (ClassName "URxvt"
                                         `And` Resource "editorterm")
             pdfEditor = renamed [ CutWordsLeft 1, CutWordsRight 11
                                 , Prepend "p(", Append ")"] $ comboL pdfsL
             codeEditor= renamed [ CutWordsLeft 1, CutWordsRight 11
                                 , Prepend "c(", Append ")"] $ comboL codeL
         in codeEditor ||| pdfEditor                            ) .
    set ("5:www"        , "~/downloads" , layoutCircle
                                      ||| layoutGrid (4/3)
                                      ||| layoutCrossD
                                      ||| layoutFullscreenFull  ) .
    set ("6:chat"       , "~/downloads" ,
         renamed [CutWordsLeft 2] $ withIM (15/100) (Resource "pspeder - skype™") $ reflectHoriz $
         renamed [CutWordsLeft 2] $ withIM (17/100) (Role "buddy_list") $ reflectHoriz $
         layoutGridD                                        ) .
    set ("7:vms"        , "~/shares"    , centerMaster (layoutMTallD
                                      ||| layoutCircle
                                      ||| layoutCrossD)     ) .
    set ("8:media"      , "~/images"    ,
         renamed [CutWordsLeft 2] $ withIM (19/100) (ClassName "gimp-dock") $ reflectHoriz $
         renamed [CutWordsLeft 2] $ withIM (25/100) (Role "gimp-toolbox")   $ reflectHoriz $
         layoutGridD             ) .
    set ("remotes"      , "~/shares"    ,
         lAddSDrawerL (ClassName "URxvt" `And` Role "drawerTerm")
                      myStandardLayout                      ) .
    set ("server"       , "~/srv"       , myStandardLayout  ) .
    set ("configs"      , "~/dev/Configs",myStandardLayout  ) $
    workspaceDir "~" myStandardLayout
    where set (n,d,l) = onWorkspace n ( workspaceDir d l )

myStandardLayout = smartBorders (layoutTallD ||| layoutMTallD ||| layoutFullscreenFull)

resizePercentage = (2/100)
layoutSpacing    = 2
layoutMasterSize = (6/10)

--------------------------
-- LAYOUTS W. DEFAULTS  --
--------------------------
layoutTallD         = layoutTall 1 layoutMasterSize
layoutMTallD        = layoutMTall 1 layoutMasterSize
layoutStackTileD    = layoutStackTile 1 (6/10)
layoutCrossD        = layoutCross (5/6)
layoutGridD         = layoutGrid (1/2)
layoutAccordionD    = layoutAccordion
layoutDishesD       = layoutDishes 1 layoutMasterSize

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
lAddSpacingD      = lAddSpacing layoutSpacing

lAddSDrawerL p l  = simpleDrawer 0 layoutMasterSize p `onLeft` l
lAddSDrawerB p l  = simpleDrawer 0 layoutMasterSize p `onBottom` l
lAddSDrawerT p l  = simpleDrawer 0 layoutMasterSize p `onTop` l
lAddSDrawerR p l  = simpleDrawer 0 layoutMasterSize p `onRight` l
lAddSpacing s l   = renamed [CutWordsLeft 2] $ spacing s $ l
lAddTabs s t l    = renamed [CutWordsLeft 2, Prepend "T(", Append ")"] $ spacing s $ addTabsAlways shrinkText t l
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
--ttt ts = (\TU.TopicDefinition {TU.tdName = n, TU.tdDir = d, TU.tdBoundLayout = l} -> (n,d,l)) ts
--topicLayoutHook :: (LayoutClass l1 a, LayoutClass l2 a) => Layout (l1 a) -> TU.TopicDefinitions -> PerWorkspace l1 l2 a
--topicLayoutHook acc (TU.TopicDefinition {TU.tdName = n, TU.tdDir = d, TU.tdBoundLayout = l}:rest)
--    | null rest = windowNavigation
--                . avoidStruts
--                . trackFloating
--                . boringWindows
--                . renamed [CutWordsLeft 1]
--                . maximize $ acc . onWorkspace n (workspaceDir d l) $ workspaceDir "~" myStandardLayout
--    | otherwise = topicLayoutHook (acc . onWorkspace n (workspaceDir d l)) rest
--
--topicLayoutHook' ts = windowNavigation
--                    . avoidStruts
--                    . trackFloating
--                    . boringWindows
--                    . renamed [CutWordsLeft 1]
--                    . maximize $
--        foldr1 (.) (fmap (\TU.TopicDefinition {TU.tdName = n, TU.tdDir = d, TU.tdBoundLayout = l} -> onWorkspace n (workspaceDir d l)) ts) $ workspaceDir "~" myStandardLayout

-- from: https://mail.haskell.org/pipermail/xmonad/2012-October/013139.html
--format (w:[]) = workspace w defaultWorkspace
--          format (w:ws) = workspace w $ format ws
--
--          workspace (name, dir, layouts) = onWorkspace name layouts $ modWorkspace name (workspaceDir dir)

--topicLayoutHook ts =
--      (foldr ($)  myStandardLayout (fmap (\TU.TopicDefinition{TU.tdName = n, TU.tdBoundLayout = l, TU.tdDir = d} -> onWorkspace n (workspaceDir d $ manage l)) ts))
--        where manage = windowNavigation
--                     . avoidStruts
--                     . trackFloating
--                     . boringWindows
--                     . renamed [CutWordsLeft 1]
--                     . maximize
      --layouts ts $ myStandardLayout
      --where layouts ts' = map (\TU.TopicDefinition{TU.tdName = n, TU.tdBoundLayout = l, TU.tdDir = d} -> onWorkspace n (workspaceDir d l)) ts'

-- topicLayoutHook' ts =
--     let layouts (td::[]) fl = onWorkspace n (workspaceDir d l) $ fl
--             where n = TU.tdName td
--                   d = TU.tdDir  td
--                   l = TU.tdBoundLayout td
--         layouts (td::ts) fl = layouts ts (onWorkspace n (workspaceDir d l) $ fl)
--             where n = TU.tdName td
--                   d = TU.tdDir  td
--                   l = TU.tdBoundLayout td
--     in windowNavigation
--      . avoidStruts
--      . trackFloating
--      . boringWindows
--      . renamed [CutWordsLeft 1]
--      . maximize
--      $ layouts ts myStandardLayout
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
