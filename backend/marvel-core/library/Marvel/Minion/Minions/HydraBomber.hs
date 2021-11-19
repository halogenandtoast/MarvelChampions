module Marvel.Minion.Minions.HydraBomber
  ( hydraBomber
  , HydraBomber(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Hp
import Marvel.Message
import Marvel.Minion.Attrs
import Marvel.Minion.Cards qualified as Cards
import Marvel.Source
import Marvel.Stats
import Marvel.Target

hydraBomber :: MinionCard HydraBomber
hydraBomber = minion HydraBomber Cards.hydraBomber (Sch 1) (Atk 1) (HP 2)

newtype HydraBomber = HydraBomber MinionAttrs
  deriving anyclass IsMinion
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage HydraBomber where
  runMessage msg (HydraBomber attrs) = HydraBomber <$> runMessage msg attrs
