{-# LANGUAGE NullaryTypeClasses, GADTs, Rank2Types, MultiParamTypeClasses, FlexibleInstances #-}
module NewTopic where

import XMonad
import XMonad.Config
import XMonad.Layout
import XMonad.Hooks.ManageDocks (avoidStruts, ToggleStruts(..))
import Data.Char                    (isDigit, digitToInt)
import Data.Monoid                  (Endo)
import XMonad.Actions.TopicSpace    (defaultTopicConfig, TopicConfig)

data Key = EZK String | XK (ButtonMask, KeySym)

data AnyLayout a = forall l. (LayoutClass l a) => AnyLayout (l a)

type Keybinding = (Key, X())


data TopicDef m where
    TD ::
        { tId :: WorkspaceId
        , tShortcut :: Key
        , tKeybindings :: [Keybinding]
        , tLayout :: AnyLayout m
        } -> TopicDef m

defaultTD = TD "" (EZK "") [] (AnyLayout $ avoidStruts Full)





--data Topic w = forall l a. LayoutClass l a => Topic
--    { ws     :: Maybe w
--    , tsconf :: TopicConfig
--    , layout :: l
--    }

--class TopicClass t where
--    ppName      :: t -> String
--    shortName   :: t -> String
--    shortcut    :: t -> Key
--    topics      :: t -> [Topic l]
--    hidden      :: t -> Bool
--    startupHook :: t -> X()
--    manageHook  :: t -> ManageHook
--
--instance TopicClass (Topic l) where
--    ppName t = name t
--    shortName t = if isDigit $ firstC then [firstC] else name t
--        where firstC = head $ name t

--data TopicDef l where
--    Topic :: { tId      :: WorkspaceId
--             , tKey     :: Key
--             , tLayout  :: AnyLayout l
--             } -> TopicDef l
--
--
--class TopicDefinition t where
--    id      :: t -> Maybe Int
--    layout  :: t -> AnyLayout a
--
--instance TopicDefinition (TopicDef l) where
--    layout Topic { tLayout = l } = AnyLayout l
--
--defaultTopic = Topic
--    { tId = "1"
--    , tKey = EZK "M"
--    , tLayout = AnyLayout $ Full ||| tiled ||| Mirror tiled
--    }
--    where tiled = Tall 1 (1/2) (3/100)
