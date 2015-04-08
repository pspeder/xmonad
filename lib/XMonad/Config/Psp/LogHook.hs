module XMonad.Config.Psp.LogHook
( pspLogHook
) where

import           Data.List                  (intercalate,elemIndex)
import           Data.Traversable           (traverse)
import           Control.Monad              (liftM,sequence)
import XMonad (withWindowSet,WindowSpace(..),X(..),Window(..))
import XMonad.StackSet (index,peek,Workspace(..))
import XMonad.Hooks.DynamicLog (PP(..),defaultPP,shorten,pad,dzenStrip,dzenColor,wrap,dzenPP,dzenEscape,dynamicLogWithPP,ppOutput)
import XMonad.Util.Run (hPutStrLn)
import XMonad.Util.Loggers (Logger(..))
import XMonad.Util.NamedWindows (getName,unName)
import XMonad.Util.NamedScratchpad (namedScratchpadFilterOutWorkspacePP)
import XMonad.Actions.TopicDefinitions (topics,TopicDefinition(..),numberedTopics)
import XMonad.Config.Psp.Topics ( myTopics,myNumberedTopics )

pspLogHook h = dynamicLogWithPP $ pspFilterOutWorkspacePP $ namedScratchpadFilterOutWorkspacePP $ myDzenPP { ppOutput = hPutStrLn h }

logTitles :: (String -> String) -> Logger
logTitles ppFocus =
        let windowTitles windowset = sequence (map (fmap showName . getName) (index windowset))
              where fw = peek windowset
                    showName nw = let window = unName nw
                                      name = show nw
                                  in if maybe False (== window) fw
                                     then ppFocus name
                                     else name
        in withWindowSet $ liftM (Just . (intercalate "|")) . windowTitles


myDzenStatus = "dzen2 -w '320' -ta 'l'" ++ myDzenStyle
myDzenConky  = "conky -c ~/.xmonad/conkyrc | dzen2 -x '320' -w '704' -ta 'r'" ++ myDzenStyle
myDzenStyle  = " -h '20' -fg '#777777' -bg '#222222' -fn 'arial:bold:size=9'"

myDzenPP = dzenPP
    { ppCurrent = dzenColor "#3399ff" "" . wrap "" "" . dzenClickWorkspace . onlyName
    , ppHidden  = dzenColor "#dddddd" "" . wrap "" "" . dzenClickWorkspace . onlyName
    , ppHiddenNoWindows = dzenColor "#777777" "" . dzenClickWorkspace . onlyName
    , ppUrgent  = dzenColor "#ff0000" "" . dzenClickWorkspace . onlyName
    , ppSep     = " "
    , ppWsSep   = "|"
    , ppLayout  = dzenColor "#aaaaaa" "" . wrap "^ca(1,xdotool key super+shift+l)·" "·^ca()"
    , ppExtras  = [windowCount]
    , ppOrder   = \(ws:l:t:e) -> [ws, l] ++ e
    , ppTitle   = dzenColor "#ffffff" ""
                    . wrap "^ca(1,xdotool key super+k)^ca(2,xdotool key super+shift+c)"
                           "                          ^ca()^ca()" . shorten 20 . dzenEscape
    }
    where onlyName ws = if (':' `elem` ws) then drop 2 ws else ws
          -- From: https://wiki.haskell.org/Xmonad/Config_archive/Nnoell's_xmonad.hs
          dzenClickWorkspace ws = "^ca(1," ++ xdo "w;" ++ xdo idx ++ ")" ++ "^ca(3," ++ xdo "w;" ++ xdo idx ++ ")" ++ ws ++ "^ca()^ca()" where
                wsIdxToString Nothing = "1"
                wsIdxToString (Just n) = show $ mod (n+1) $ length myTopics
                idx = wsIdxToString (elemIndex ws myTopics)
                xdo key = "/usr/bin/xdotool key super+" ++ key

--winCount :: String -> String
--winCount = show (length (withWindowSet $ W.allWindows))

{-
windowCount2 :: String
windowCount2 = let windows =  fmap (\W.Workspace {W.stack = s} -> W.integrate' s) $ withWindowSet $ W.workspaces
                   numWindows = length windows
               in show numWindows

--}

-- | From: https://github.com/bobtwinkles/dotfiles/blob/master/xmonad/xmonad.hs
windowCount = withWindowSet $ fmap Just . (\x -> let allWindows = index x
                                                     focused = peek x
                                                     numWindows = length allWindows
                                                     listWindowTitles :: [Window] -> X [String]
                                                     listWindowTitles xs = traverse (fmap show . getName) xs
                                                     windows = case focused of Nothing -> []
                                                                               Just x  -> filter (\y -> not ( Just y == focused ) ) allWindows
                                                     titles = listWindowTitles windows
                                                 in fmap (formatTitles numWindows) titles)

formatTitle s n = s ++ "(" ++ show n ++ ") "

formatTitles num (x:xs) = formatTitle x num ++ formatTitles num xs
formatTitles _ [] = ""

-- Loghook
myOldLogHook h = dynamicLogWithPP $ defaultPP
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

pspFilterOutWorkspacePP :: PP -> PP
pspFilterOutWorkspacePP pp = pp {
  ppSort = fmap (. pspFilterOutWorkspace) (ppSort pp)
  }

pspFilterOutWorkspace :: [WindowSpace] -> [WindowSpace]
pspFilterOutWorkspace = filter (\(Workspace tag _ _) -> tag `elem` myNumberedTopics) -- TODO : instead check if tdHidden is True
