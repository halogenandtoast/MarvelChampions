module Marvel.Minion.Minions.Shocker where

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

shocker :: MinionCard Shocker
shocker = minion Shocker Cards.shocker (Sch 1) (Atk 2) (HP 3)

newtype Shocker = Shocker MinionAttrs
  deriving anyclass IsMinion
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage Shocker where
  runMessage msg (Shocker attrs) = Shocker <$> runMessage msg attrs
