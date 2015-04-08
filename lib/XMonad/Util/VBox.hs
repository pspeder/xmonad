module XMonad.Util.VBox where

import Data.Monoid(mempty, mappend, All(..))
import Data.List ((\\))
import Data.Map (fromList)
import Text.Regex.PCRE((=~))        -- This requires the package regex-pcre from cabal

import XMonad (title,doShift,liftX,(-->),composeAll)
import XMonad.Util.WindowProperties (Property(..),propertyToQuery)
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace)

vboxManageHook = propertyToQuery (ClassName "VirtualBox") -->
                    do name <- title
                       case (name =~ "( \\(.*\\))?( \\[[^\\]]+\\])? - Oracle VM VirtualBox$") :: (String,String,String)
                         of (_,"",_) -> return mempty
                            (n,_,_)  -> do let ws = "vm-" ++ n
                                           liftX $ addHiddenWorkspace ws
                                           doShift ws ]
