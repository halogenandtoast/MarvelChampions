module Marvel.Minion.Minions.Sandman
  ( sandman
  , Sandman(..)
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

sandman :: MinionCard Sandman
sandman = minion Sandman Cards.sandman (Sch 2) (Atk 3) (HP 4)

newtype Sandman = Sandman MinionAttrs
  deriving anyclass IsMinion
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage Sandman where
  runMessage msg (Sandman attrs) = Sandman <$> runMessage msg attrs
