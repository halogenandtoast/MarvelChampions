module Marvel.Minion.Minions.Killmonger
  ( killmonger
  , Killmonger(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types
import Marvel.Trait

killmonger :: MinionCard Killmonger
killmonger = minion Killmonger Cards.killmonger (Sch 2) (Atk 2) (HP 5)

newtype Killmonger = Killmonger MinionAttrs
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage Killmonger where
  runMessage msg m@(Killmonger attrs) = case msg of
    MinionMessage minionId msg' | minionId == toId attrs -> case msg' of
      MinionDamaged (UpgradeSource uid) _ -> do
        isBlackPanther <- upgradeMatches (UpgradeWithTrait BlackPanther) uid
        if isBlackPanther then pure m else Killmonger <$> runMessage msg attrs
      _ -> Killmonger <$> runMessage msg attrs
    _ -> Killmonger <$> runMessage msg attrs
