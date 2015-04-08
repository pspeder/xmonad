module XMonad.Actions.TopicDefinitions.TopicSpace
( module TS
) where
import qualified XMonad.Actions.TopicSpace as TS hiding (
    -- We need to hide stuff that we overwrite
    TopicConfig (..)
  , defaultTopicConfig
  , 
)
