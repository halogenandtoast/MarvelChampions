module Marvel.Upgrade.Upgrades.DefenseSpecialist where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

defenseSpecialist :: UpgradeCard DefenseSpecialist
defenseSpecialist = upgrade DefenseSpecialist Cards.defenseSpecialist

newtype DefenseSpecialist = DefenseSpecialist (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities DefenseSpecialist where
  getAbilities a = [limitedWindowAbility a 1 (W.MakesBasicDefense W.After You AnyEnemy) HeroResponse OwnsThis ExhaustCost (runAbility a 1)]

instance HasModifiersFor DefenseSpecialist where
  getModifiersFor _ (IdentityRef you) (DefenseSpecialist attrs) | you == upgradeController attrs = do
    hero <- identityMatches HeroIdentity you
    pure [DefenseModifier 1 | hero]
  getModifiersFor _ _ _ = pure []

instance RunMessage DefenseSpecialist where
  runMessage msg a@(DefenseSpecialist attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> a <$ drawCards you 1
    _ -> DefenseSpecialist <$> runMessage msg attrs
