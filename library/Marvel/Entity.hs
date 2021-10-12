module Marvel.Entity where

class Entity a where
  type EntityId a
  toId :: a -> EntityId a
