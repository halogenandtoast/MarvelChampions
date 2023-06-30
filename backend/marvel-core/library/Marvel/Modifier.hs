module Marvel.Modifier (module Marvel.Modifier, module Marvel.Modifier.Types) where

import Marvel.Prelude

import Marvel.Game.Source
import Marvel.Modifier.Types
import Marvel.Ref

class HasModifiersFor a where
  getModifiersFor :: (HasGame m) => Source -> Target -> a -> m [Modifier]
  getModifiersFor _ _ _ = pure []
