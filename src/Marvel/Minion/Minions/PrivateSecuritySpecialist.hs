module Marvel.Minion.Minions.PrivateSecuritySpecialist (
  privateSecuritySpecialist,
  PrivateSecuritySpecialist (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Environment.Environments.CriminalEnterprise (infamyOrMadness)

privateSecuritySpecialist :: MinionCard PrivateSecuritySpecialist
privateSecuritySpecialist = minion PrivateSecuritySpecialist Cards.privateSecuritySpecialist (Sch 0) (Atk 1) (HP 4)

newtype PrivateSecuritySpecialist = PrivateSecuritySpecialist (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PrivateSecuritySpecialist where
  runMessage msg e@(PrivateSecuritySpecialist attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target _) | isTarget attrs target -> e <$ infamyOrMadness 1
    _ -> PrivateSecuritySpecialist <$> runMessage msg attrs
