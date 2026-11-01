module Marvel.Ally.Allies.Armor where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Entity ()
import Marvel.Message.Lifted ()
import Marvel.Modifier ()
import Marvel.Ref ()
import Marvel.Card.Code ()

armor :: AllyCard Armor
armor = ally Armor Cards.armor (Thw 1, 1) (Atk 1, 1) (HP 2)

newtype Armor = Armor (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Armor where
  runMessage msg (Armor attrs) = runQueueT $ Armor <$> runMessage msg attrs
