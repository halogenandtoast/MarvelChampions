module Marvel.Projection where

import Marvel.Prelude

import Marvel.Entity

projectP :: (Functor m, Projection m a) => Field a typ -> (typ -> Bool) -> Id a -> m Bool
projectP = projectMap

projectMap :: (Functor m, Projection m a) => Field a typ -> (typ -> b) -> Id a -> m b
projectMap fld f ident = f <$> project fld ident

class Entity a => Projection m a where
  project :: Field a typ -> Id a -> m typ

