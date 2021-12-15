module Marvel.Minion.Minions.Whiplash
  ( whiplash
  , Whiplash(..)
  )
where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Hp
import Marvel.Message
import Marvel.Minion.Attrs
import Marvel.Minion.Cards qualified as Cards

whiplash :: MinionCard Whiplash
whiplash = minion Whiplash Cards.whiplash (Sch 2) (Atk 3) (HP 4)

newtype Whiplash = Whiplash MinionAttrs
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage Whiplash where
  runMessage msg (Whiplash attrs) =
    Whiplash <$> runMessage msg attrs
