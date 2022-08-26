module Marvel.Entity where

import Marvel.Prelude

import Marvel.Id
import Marvel.Target

class Exhaustable a where
  isExhausted :: a -> Bool

class HasController a where
  controller :: a -> IdentityId

class Entity a where
  type Id a
  data Attrs a
  data family Field a :: Type -> Type
  toId :: a -> Id a
  toAttrs :: a -> Attrs a
  field :: Field a typ -> a -> typ

-- field :: MonadGame env m => Field a typ -> Id a -> m typ
-- field = undefined

fieldP :: Field a typ -> (typ -> Bool) -> a -> Bool
fieldP = undefined

isTarget :: IsTarget a => a -> Target -> Bool
isTarget a = (== toTarget a)
