module Marvel.Modifier where

import Marvel.Prelude

import Marvel.Game.Source
import Marvel.Keyword
import Marvel.Source
import Marvel.Target
import Marvel.Trait

data Modifier
  = ResourceCostReduction Natural
  | ThwartModifier Natural
  | AttackModifier Natural
  | SchemeModifier Natural
  | DefenseModifier Natural
  | AllyLimitModifier Natural
  | HitPointModifier Natural
  | HandSizeModifier Natural
  | TraitModifier Trait
  | KeywordModifier Keyword
  | LastSpecial
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

class HasModifiersFor a where
  getModifiersFor :: MonadGame env m => Source -> Target -> a -> m [Modifier]
  getModifiersFor _ _ _ = pure []
