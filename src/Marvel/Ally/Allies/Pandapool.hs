module Marvel.Ally.Allies.Pandapool where

import Marvel.Prelude
import Marvel.Ability (HasAbilities)
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Stats ()

pandapool :: AllyCard Pandapool
pandapool = ally Pandapool Cards.pandapool (Thw 0, 0) (Atk 3, 0) (HP 4)

newtype Pandapool = Pandapool (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Pandapool where
  runMessage msg (Pandapool attrs) = runQueueT $ Pandapool <$> runMessage msg attrs
