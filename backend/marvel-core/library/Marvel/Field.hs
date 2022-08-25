module Marvel.Field where

import Marvel.Prelude

import Marvel.Entity
import {-# SOURCE #-} Marvel.Game

data family Field a :: Type -> Type

field :: MonadGame env m => Field a typ -> EntityId a -> m typ
field = undefined

fieldP :: MonadGame env m => Field a typ -> (typ -> Bool) -> EntityId a -> m Bool
fieldP = undefined
