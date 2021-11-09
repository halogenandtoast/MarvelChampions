{-# LANGUAGE UndecidableInstances #-}
module Marvel.Villain.Villains.Rhino where

import Marvel.Prelude

import Marvel.GameValue
import Marvel.Hp
import Marvel.Message
import Marvel.Stats
import Marvel.Villain.Attrs
import qualified Marvel.Villain.Cards as Cards

newtype Rhino = Rhino VillainAttrs
  deriving anyclass IsVillain
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rhino :: VillainCard Rhino
rhino = villain Rhino Cards.rhino (Sch 1) (Atk 2) (HP $ PerPlayer 14)

instance RunMessage Rhino where
  runMessage msg (Rhino attrs) = Rhino <$> runMessage msg attrs
