module Marvel.Entity where

import Marvel.Id
import Marvel.Prelude

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
