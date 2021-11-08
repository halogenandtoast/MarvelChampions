{-# LANGUAGE UndecidableInstances #-}
module Marvel.Ally.Allies.MariaHill where

import Marvel.Prelude

import Marvel.Ally.Attrs
import qualified Marvel.Ally.Cards as Cards
import Marvel.Card.Code
import Marvel.Entity

mariaHill :: AllyCard MariaHill
mariaHill = ally MariaHill Cards.mariaHill (Thw 2) (Atk 1)

newtype MariaHill = MariaHill AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity)
