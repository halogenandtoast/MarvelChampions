module Marvel.Modifier where

import Marvel.Prelude

import GHC.Generics
import Marvel.Game.Source
import Marvel.Source
import Marvel.Target

data Modifier
  = ResourceCostReduction Natural
  | ThwartModifier Natural
  | AttackModifier Natural
  | SchemeModifier Natural
  | AllyLimitModifier Natural
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

class HasModifiersFor a where
  getModifiersFor :: MonadGame env m => Source -> Target -> a -> m [Modifier]
  getModifiersFor _ _ _ = pure []

class HasModifiersFor' f where
  getModifiersFor' :: MonadGame env m => Source -> Target  -> f p -> m [Modifier]

genericGetModifiersFor
  :: (MonadGame env m, HasModifiersFor' (Rep a), Generic a)
  => Source
  -> Target
  -> a
  -> m [Modifier]
genericGetModifiersFor source target = getModifiersFor' source target . from

instance HasModifiersFor' f => HasModifiersFor' (M1 i c f) where
  getModifiersFor' source target = getModifiersFor' source target . unM1

instance (HasModifiersFor' l, HasModifiersFor' r) => HasModifiersFor' (l :+: r) where
  getModifiersFor' source target = \case
    L1 x -> getModifiersFor' source target x
    R1 x -> getModifiersFor' source target x

instance HasModifiersFor c => HasModifiersFor' (K1 i c) where
  getModifiersFor' source target = getModifiersFor source target . unK1
