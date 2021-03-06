module Marvel.Entity where

import Marvel.Id
import Marvel.Prelude

import GHC.Generics

class Exhaustable a where
  isExhausted :: a -> Bool

class HasController a where
  controller :: a -> IdentityId

class Entity a where
  type EntityId a
  type EntityAttrs a
  toId :: a -> EntityId a
  toAttrs :: a -> EntityAttrs a

instance Entity a => Entity (With a b) where
  type EntityId (With a b) = EntityId a
  type EntityAttrs (With a b) = EntityAttrs a
  toId (With a _) = toId a
  toAttrs (With a _) = toAttrs a

class HasAttrs' attrs f where
  toAttrs' :: f p -> attrs

genericToAttrs :: (Generic a, HasAttrs' attrs (Rep a)) => a -> attrs
genericToAttrs = toAttrs' . from

instance HasAttrs' attrs f => HasAttrs' attrs (M1 i c f) where
  toAttrs' = toAttrs' . unM1

instance (HasAttrs' attrs l, HasAttrs' attrs r) => HasAttrs' attrs (l :+: r) where
  toAttrs' = \case
    L1 x -> toAttrs' x
    R1 x -> toAttrs' x

instance (Entity c, attrs ~ EntityAttrs c) => HasAttrs' attrs (K1 i c) where
  toAttrs' = toAttrs . unK1
