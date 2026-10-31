module Marvel.Ally.Allies.ControlledMinion where

import Marvel.Prelude
import Marvel.Ability.Types (HasAbilities)
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

-- | An encounter minion treated as an ally. Its printed powers and physical card
-- are supplied by the conversion, while its printed text has no abilities.
controlledMinion :: AllyCard ControlledMinion
controlledMinion = ally ControlledMinion Cards.controlledMinion (Thw 0, 1) (Atk 0, 1) (HP 0)

newtype ControlledMinion = ControlledMinion (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage ControlledMinion where
  runMessage msg (ControlledMinion attrs) = ControlledMinion <$> runMessage msg attrs
