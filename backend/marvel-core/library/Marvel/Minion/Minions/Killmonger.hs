module Marvel.Minion.Minions.Killmonger (
  killmonger,
  Killmonger (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Trait

killmonger :: MinionCard Killmonger
killmonger = minion Killmonger Cards.killmonger (Sch 2) (Atk 2) (HP 5)

newtype Killmonger = Killmonger (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage Killmonger where
  runMessage msg m@(Killmonger attrs) = case msg of
    MinionMessage ident msg' | ident == minionId attrs -> case msg' of
      MinionDamaged (UpgradeRef uid) _ -> do
        isBlackPanther <- upgradeMatches (UpgradeWithTrait BlackPanther) uid
        if isBlackPanther then pure m else Killmonger <$> runMessage msg attrs
      _ -> Killmonger <$> runMessage msg attrs
    _ -> Killmonger <$> runMessage msg attrs
