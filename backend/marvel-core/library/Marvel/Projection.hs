module Marvel.Projection where

import Marvel.Game.Source
import Marvel.Prelude
import Marvel.Entity

projectP :: MonadGame env m => Field a typ -> (typ -> Bool) -> Id a -> m Bool
projectP = undefined

class Entity a => Projection a where
  project :: Field a typ -> Id a -> typ

