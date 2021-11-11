module Marvel.Hp where

import Marvel.Prelude

import GHC.Generics
import Marvel.GameValue

newtype HP = HP { unHp :: GameValue }
  deriving newtype (Show, Eq, ToJSON, FromJSON)

class HasStartingHP a where
  startingHP :: a -> HP

class HasStartingHP' f where
  startingHP' :: f a -> HP

instance HasStartingHP' f => HasStartingHP' (M1 i c f) where
  startingHP' = startingHP' . unM1

instance (HasStartingHP' l, HasStartingHP' r) => HasStartingHP' (l :+: r) where
  startingHP' = \case
    L1 x -> startingHP' x
    R1 x -> startingHP' x

instance HasStartingHP c => HasStartingHP' (K1 i c) where
  startingHP' = startingHP . unK1

genericHasStartingHP :: (HasStartingHP' (Rep a), Generic a) => a -> HP
genericHasStartingHP = startingHP' . from
