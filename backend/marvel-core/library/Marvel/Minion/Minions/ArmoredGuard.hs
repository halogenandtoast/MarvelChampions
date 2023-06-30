module Marvel.Minion.Minions.ArmoredGuard (
  armoredGuard,
  ArmoredGuard (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Hp
import Marvel.Message
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

armoredGuard :: MinionCard ArmoredGuard
armoredGuard = minion ArmoredGuard Cards.armoredGuard (Sch 0) (Atk 1) (HP 3)

newtype ArmoredGuard = ArmoredGuard (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage ArmoredGuard where
  runMessage msg (ArmoredGuard attrs) = ArmoredGuard <$> runMessage msg attrs
