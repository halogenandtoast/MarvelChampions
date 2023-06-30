module Marvel.Entity where

import Marvel.Prelude

import Marvel.Id
import Marvel.Ref

class Exhaustable a where
  isExhausted :: a -> Bool

class HasController a where
  controller :: a -> IdentityId

class Entity a where
  type Id a
  data Attrs a
  data Field a :: Type -> Type
  toId :: a -> Id a
  toAttrs :: a -> Attrs a
  field :: Field a typ -> a -> typ

-- field :: MonadGame env m => Field a typ -> Id a -> m typ
-- field = undefined

fieldP :: (Entity a) => Field a typ -> (typ -> Bool) -> a -> Bool
fieldP fld f = f . field fld

isTarget :: (IsRef a) => a -> Target -> Bool
isTarget a = (== toTarget a)
