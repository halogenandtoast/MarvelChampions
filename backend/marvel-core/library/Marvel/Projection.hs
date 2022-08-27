module Marvel.Projection where

import Marvel.Prelude

import Marvel.Entity

projectP :: Field a typ -> (typ -> Bool) -> Id a -> m Bool
projectP = error "unimplemented"

class Entity a => Projection a where
  project :: Field a typ -> Id a -> typ

