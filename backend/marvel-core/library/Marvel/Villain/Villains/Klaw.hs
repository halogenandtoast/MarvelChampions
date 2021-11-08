{-# LANGUAGE UndecidableInstances #-}
module Marvel.Villain.Villains.Klaw where

import Marvel.Prelude

import Marvel.GameValue
import Marvel.Stats
import Marvel.Villain.Attrs
import qualified Marvel.Villain.Cards as Cards

newtype Klaw = Klaw VillainAttrs
  deriving anyclass IsVillain
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

klaw :: VillainCard Klaw
klaw = villain Klaw Cards.klaw (Sch 2) (Atk 0) (PerPlayer 12)
