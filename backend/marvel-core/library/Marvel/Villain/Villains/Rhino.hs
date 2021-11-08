{-# LANGUAGE UndecidableInstances #-}
module Marvel.Villain.Villains.Rhino where

import Marvel.Prelude

import Marvel.GameValue
import Marvel.Stats
import Marvel.Villain.Attrs
import qualified Marvel.Villain.Cards as Cards

newtype Rhino = Rhino VillainAttrs
  deriving anyclass IsVillain
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rhino :: VillainCard Rhino
rhino = villain Rhino Cards.rhino (Sch 1) (Atk 2) (PerPlayer 14)
