module Marvel.Entity where

import Marvel.Id
import Marvel.Prelude

class Exhaustable a where
  isExhausted :: a -> Bool

class HasController a where
  controller :: a -> IdentityId

class Entity a where
  type EntityId a
  data EntityAttrs a
  toId :: a -> EntityId a
  toAttrs :: a -> EntityAttrs a
