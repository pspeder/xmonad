module PSP.Constants
(-- myBorderWidth
--, myNormalBorderColor
--, myFocusedBorderColor
--, myClickJustFocuses
--, myFocusFollowsMouse
--, myModMask
myTerminal
, myShell
, myBrowser
, myBrowserNewWindow
, myBrowserPrivateWin
, myEditor
, myPDFViewer
, myMailClient
) where

-- Windows
myFloatingAppProps  = []
myCFloats           = []
myFullFloats        = []

-- Applications
myTerminal :: String
myTerminal = "urxvtc -e " ++ myShell

myShell :: String
myShell = "/bin/zsh"

myBrowser :: String
myBrowser = "firefox"

myBrowserPrivateWin :: String
myBrowserPrivateWin = "firefox -private-window "

myBrowserNewWindow :: String
myBrowserNewWindow = "firefox -new-tab "

myEditor :: String
myEditor = "gvim"

myPDFViewer :: String
myPDFViewer = "mupdf"

myMailClient :: String
myMailClient = "thunderbird"

