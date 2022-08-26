module Marvel.Minion.Minions.Whiplash
  ( whiplash
  , Whiplash(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Hp
import Marvel.Message
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

whiplash :: MinionCard Whiplash
whiplash = minion Whiplash Cards.whiplash (Sch 2) (Atk 3) (HP 4)

newtype Whiplash = Whiplash (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance RunMessage Whiplash where
  runMessage msg (Whiplash attrs) = Whiplash <$> runMessage msg attrs
