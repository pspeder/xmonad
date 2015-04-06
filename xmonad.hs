-- xmonad.hs
-- Main configuration file for Paw Saabye's XMonad setup.
import XMonad
import XMonad.Core
import Data.Monoid (mconcat)
import qualified XMonad.StackSet as W
import Control.Monad (liftM,sequence)
import XMonad.Util.NamedWindows (getName, unName)
import XMonad.Util.Loggers
import Data.List (intercalate,elemIndex)
import Data.Traversable (traverse, fmapDefault)
import Control.Monad.Writer (All(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Util.Run (spawnPipe,safeSpawn,hPutStrLn)
import XMonad.Actions.TopicSpace (checkTopicConfig)
import XMonad.Util.EZConfig (additionalKeysP,removeKeysP)
import XMonad.Util.WindowProperties (Property(..))
import XMonad.Hooks.ManageDocks (manageDocks)
import XMonad.Hooks.InsertPosition (insertPosition,Focus(..),Position(..))
import XMonad.Hooks.DynamicHooks (dynamicMasterHook)
import XMonad.Actions.GridSelect (defaultGSConfig) --Will be replaced by something in PSP.Configs
import XMonad.Layout.LayoutHints (hintsEventHook)
import XMonad.Layout.Maximize (maximizeRestore)
import XMonad.Actions.RotSlaves
import XMonad.Actions.Commands (runCommand)
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import XMonad.Prompt.AppendFile
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Man (Man(..),manPrompt)
import XMonad.Layout.WorkspaceDir(changeDir)
import XMonad.Hooks.DynamicLog
--import XMonad.Hooks.DynamicLog (PP, ppSort) -- for filtering
import XMonad.Util.Cursor (setDefaultCursor)
import CustomUrgencyHook
import PScratchpads (pspNamedScratchpadManageHook,namedScratchpadFilterOutWorkspacePP)
import PSP.Keys
import PSP.Topics
import PSP.Configs (myXPConfig)
import PSP.Utils (goToSelectedWS,spawnIn)

main :: IO()
main = do
    d <- spawnPipe "dzen2 -p -ta l -e 'onstart=lower'"
    checkTopicConfig myTopics myTopicConfig
    xmonad $ myConfig d

urxvtEventHook e = case e of
    PropertyEvent { ev_window = w } -> do isURXVT <- runQuery (className =? "URxvt") w
                                          if not isURXVT then hintsEventHook e
                                                         else return (All True)
    _                               -> return (All True)

startupApps = [spawn "urxvt"]--[spawn "pidgin"], safeSpawn "apulse32" ["skype"]]

topicStartupHook = ewmhDesktopsStartup <+> setWMName "LG3D" <+> mconcat startupApps

myConfig d =
    withUrgencyHookC LibNotifyUrgencyHook
                     urgencyConfig { suppressWhen = OnScreen
                                   , remindWhen = Repeatedly 5 90 } $
    ewmh $ defaultConfig
        { modMask           = mod4Mask
        , terminal          = "urxvtcd"
        , workspaces        = myTopics
        , startupHook       = topicStartupHook <+> setDefaultCursor xC_left_ptr
        , handleEventHook   = ewmhDesktopsEventHook <+> fullscreenEventHook <+> urxvtEventHook
        , manageHook        = manageHook defaultConfig <+> dynamicMasterHook <+>
                              insertPosition Below Newer <+> manageDocks <+>
                              pspNamedScratchpadManageHook <+>
                              myManageHook [(ClassName "Gimp")]     -- ts
                                           []                       -- fs
                                           [(ClassName "Skype" `And` Title "Options")] -- cfs
                                           []                       -- ffs
                                           []                       -- is
                                           []                       -- mss
        , layoutHook        = myTopicLayoutHook
        , logHook           = logHook defaultConfig >> ewmhDesktopsLogHook >> myLogHook d
        }
        `removeKeysP` (
        [ "M-<Space>"       -- Originally layout toggle
        , "M-S-<Space>"     --
        , "M-m"
        , "M-w"             -- First screen
        , "M-S-w"           -- Move to first screen
        , "M-e"             -- Second screen
        , "M-S-e"           -- Move to second screen
        , "M-r"             -- Third screen
        , "M-S-r"           -- Move to Third screen
        , "M-p"             -- Run program with dmenu
        , "M-S-p"           -- Run program with gmrun
        , "M-S-<Return>"    -- Spawn terminal
        , "M-<Return>"      -- Reset layout
        , "M-S-/"           -- Show keybinding
        , "M-q"             -- Restart XMonad
        , "M-z"
        ] ++ [ ("M-" ++ s ++ [n]) | n <- show [1..9]
                                  , s <- ["S-", ""] ])
        `additionalKeysP` (myTopicKeys (
        [ ("M-<Return>"             , spawnShell                                )
        , ("M-p s"                  , sshPrompt myXPConfig                      )
        , ("M-p d"                  , changeDir myXPConfig                      )
        , ("M-p x"                  , xmonadPrompt myXPConfig                   )
        , ("M-p m"                  , manPrompt myXPConfig                      )
        , ("M-p n"                  , do spawnIn "/home/psp/notes/" "date >> quicknote.txt"
                                         appendFilePrompt myXPConfig "/home/psp/notes/quicknote.txt")
        , ("M-r"                    , runOrRaisePrompt myXPConfig               )
        , ("M-<Tab>"                , rotAllUp                                  )
        , ("M-S-<Tab>"              , rotAllDown                                )
        , ("M-w"                    , goToSelectedWS myTopicConfig True defaultGSConfig)
        , ("M-y"                    , keysCommands >>= runCommand                   )
        , ("M-S-l"                  , sendMessage NextLayout                    )
        , ("M-f"                    , withFocused $ sendMessage.maximizeRestore ) -- make window full screen
        --, ("M-m"                    , windows W.focusMaster                     ) -- focus master window
        --, ("M-S-m"                  , windows W.swapMaster                      ) -- make current master window
        -- | Screen Brightness
        , ("XF86MonBrightnessDown"  , spawn "xbacklight -dec 8%"                )
        , ("XF86MonBrightnessUp"    , spawn "xbacklight -inc 8%"                )
        ] ++ keysNamedScratchpads ++ powerMenu))

myLogHook h = dynamicLogWithPP $ pspFilterOutWorkspacePP $ namedScratchpadFilterOutWorkspacePP $ myDzenPP { ppOutput = hPutStrLn h }

logTitles :: (String -> String) -> Logger
logTitles ppFocus =
        let windowTitles windowset = sequence (map (fmap showName . getName) (W.index windowset))
                where
                    fw = W.peek windowset
                    showName nw =
                        let
                            window = unName nw
                            name = show nw
                        in
                            if maybe False (== window) fw
                            then ppFocus name
                            else name
        in withWindowSet $ liftM (Just . (intercalate "|")) . windowTitles

pspFilterOutWorkspace :: [WindowSpace] -> [WindowSpace]
pspFilterOutWorkspace = filter (\(W.Workspace tag _ _) -> tag `elem` myNumberedTopics) -- TODO : instead check if tdHidden is True

pspFilterOutWorkspacePP :: PP -> PP
pspFilterOutWorkspacePP pp = pp {
  ppSort = fmap (. pspFilterOutWorkspace) (ppSort pp)
  }

myDzenStatus = "dzen2 -w '320' -ta 'l'" ++ myDzenStyle
myDzenConky  = "conky -c ~/.xmonad/conkyrc | dzen2 -x '320' -w '704' -ta 'r'" ++ myDzenStyle
myDzenStyle  = " -h '20' -fg '#777777' -bg '#222222' -fn 'arial:bold:size=9'"

myDzenPP  = dzenPP
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
          dzenClickWorkspace ws = "^ca(1," ++ xdo "w;" ++ xdo index ++ ")" ++ "^ca(3," ++ xdo "w;" ++ xdo index ++ ")" ++ ws ++ "^ca()^ca()" where
			wsIdxToString Nothing = "1"
			wsIdxToString (Just n) = show $ mod (n+1) $ length myTopics
			index = wsIdxToString (elemIndex ws myTopics)
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
windowCount = withWindowSet $ fmap Just . (\x -> let allWindows = W.index x
                                                     focused = W.peek x
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
