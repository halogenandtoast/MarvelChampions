module Marvel.Ally.Allies.ColossusMagik where

import Marvel.Prelude
import Marvel.Hand ()
import Marvel.GameValue ()
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

colossusMagik :: AllyCard ColossusMagik
colossusMagik = ally ColossusMagik Cards.colossusMagik (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype ColossusMagik = ColossusMagik (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage ColossusMagik where
  runMessage msg (ColossusMagik attrs) = runQueueT $ ColossusMagik <$> runMessage msg attrs
