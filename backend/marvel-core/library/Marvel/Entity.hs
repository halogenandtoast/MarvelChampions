module Marvel.Entity where

import Marvel.Prelude

import Marvel.Id
import {-# SOURCE #-} Marvel.Game
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

field :: MonadGame env m => Field a typ -> Id a -> m typ
field = undefined

fieldP :: MonadGame env m => Field a typ -> (typ -> Bool) -> Id a -> m Bool
fieldP = undefined

isTarget :: IsTarget a => a -> Target -> Bool
isTarget a = (== toTarget a)
