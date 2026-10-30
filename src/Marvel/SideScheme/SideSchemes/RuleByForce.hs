module Marvel.SideScheme.SideSchemes.RuleByForce where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

ruleByForce :: SideSchemeCard RuleByForce
ruleByForce = sideScheme RuleByForce Cards.ruleByForce (Static 4)

newtype RuleByForce = RuleByForce (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor RuleByForce where
  getModifiersFor _ target (RuleByForce attrs) | isTarget attrs target = do
    present <- selectAny (MinionIs Minions.luciaVonBardas)
    pure [if present then HazardIconModifier 1 else AccelerationIconModifier 1]
  getModifiersFor _ _ _ = pure []

instance RunMessage RuleByForce where
  runMessage msg (RuleByForce attrs) = RuleByForce <$> runMessage msg attrs
