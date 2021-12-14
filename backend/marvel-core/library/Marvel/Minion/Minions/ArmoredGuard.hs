module Marvel.Minion.Minions.ArmoredGuard
  ( armoredGuard
  , ArmoredGuard(..)
  )
where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Hp
import Marvel.Message
import Marvel.Minion.Attrs
import Marvel.Minion.Cards qualified as Cards

armoredGuard :: MinionCard ArmoredGuard
armoredGuard = minion ArmoredGuard Cards.armoredGuard (Sch 0) (Atk 1) (HP 3)

newtype ArmoredGuard = ArmoredGuard MinionAttrs
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage ArmoredGuard where
  runMessage msg (ArmoredGuard attrs) =
    ArmoredGuard <$> runMessage msg attrs
