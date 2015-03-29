module PScratchpads
( pspNamedScratchpadManageHook
, NamedScratchpads
, namedScratchpadAction
, myScratches
) where

import           XMonad
import qualified XMonad.StackSet                    as W
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Types (Direction2D(..))
--import           XMonad.ManageHook ((=?),resource,stringProperty)
--import           XMonad.Util.WindowProperties

type NamedScrathpads = [NamedScratchpad]

myScratches :: NamedScrathpads
myScratches =
    [ NS { name  = "term"
         , cmd   = "urxvtc -name 'scratchterm'"
         -- screen -c ?
         , query = resource =? "scratchterm"
         , hook  = customFloating $ W.RationalRect (0.5/6) (1.9/6) (5/6) (4/6)
         }
    , NS { name  = "notes"
         , cmd   = "gvim -name 'scratchnotepad' --role 'scratchnotepad' -n ~/notes/note.md"
         , query = wmrole =? "scratchnotepad"
         , hook  = popOut L (1/3)
         }
    , NS { name  = "tasks"
         , cmd   = "urxvtc -name 'scratchtasks' -e 'vit'"
         , query = resource =? "scratchtasks"
         , hook  = customFloating $ W.RationalRect (0.5/6) (1.9/6) (5/6) (4/6)
         }
    , NS { name  = "editor"
         , cmd   = "gvim -name 'scratcheditor' --role 'scratcheditor' -geometry '140x50'"
         , query = wmrole =? "scratcheditor"
         , hook  = customFloating $ W.RationalRect (0.5/6) (1.9/6) (5/6) (4/6)
         }
    , NS { name  = "ghci"
         , cmd   = "urxvtc -name 'scratchghci' -e 'ghci'"
         , query = resource =? "scratchghci"
         , hook  = popOut D (2/5)
         }
    , NS { name  = "mixer"
         , cmd   = "urxvtc -name 'scratchmixer' -e 'alsamixer'"
         , query = resource =? "scratchmixer"
         , hook  = popOut U (4/10)
         }
    , NS { name  = "htop"
         , cmd   = "urxvtc -name 'scratchhtop' -e 'htop'"
         , query = resource =? "scratchhtop"
         , hook  = popOut D (1/2)
         }
    , NS { name  = "mail"
         , cmd   = "urxvtc -'scratchmail' -name 'scratchmail' -e 'mutt'"
         , query = resource =? "scratchmail"
         , hook  = popOut U (1/2)
         }
    , NS { name  = "music"
         , cmd   = "urxvtc -name 'scratchmusic' -e 'ncmpcpp'"
         , query = resource =? "scratchmusic"
         , hook  = defaultFloating
         }
    ]
    where wmrole = stringProperty "WM_WINDOW_ROLE"

pspNamedScratchpadManageHook :: ManageHook
pspNamedScratchpadManageHook = namedScratchpadManageHook myScratches

popOut :: Direction2D -> Rational -> ManageHook
popOut direction s = case direction of U -> customFloating $ W.RationalRect   0  (1-s) 1 s
                                       D -> customFloating $ W.RationalRect   0    0   1 s
                                       R -> customFloating $ W.RationalRect   0    0   s 1
                                       L -> customFloating $ W.RationalRect (1-s)  0   s 1

