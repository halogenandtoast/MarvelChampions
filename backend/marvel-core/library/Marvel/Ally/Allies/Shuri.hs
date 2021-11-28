module Marvel.Ally.Allies.Shuri
  ( shuri
  , Shuri(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Attrs
import Marvel.Ally.Cards qualified as Cards
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Hp
import Marvel.Message
import Marvel.Modifier
import Marvel.Source
import Marvel.Stats
import Marvel.Target

shuri :: AllyCard Shuri
shuri = ally Shuri Cards.shuri (Thw 1, 1) (Atk 1, 1) (HP 3)

newtype Shuri = Shuri AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities Shuri where
  getAbilities _ = []

instance RunMessage Shuri where
  runMessage msg (Shuri attrs) = Shuri <$> runMessage msg attrs
