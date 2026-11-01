module Marvel.Upgrade.Upgrades.CombatSpecialist where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

combatSpecialist :: UpgradeCard CombatSpecialist
combatSpecialist = upgrade CombatSpecialist Cards.combatSpecialist

newtype CombatSpecialist = CombatSpecialist (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities CombatSpecialist where
  getAbilities a = [limitedWindowAbility a 1 (W.MakesBasicAttack W.After You) HeroResponse OwnsThis ExhaustCost (runAbility a 1)]

instance HasModifiersFor CombatSpecialist where
  getModifiersFor _ (IdentityRef you) (CombatSpecialist attrs) | you == upgradeController attrs = do
    hero <- identityMatches HeroIdentity you
    pure [AttackModifier 1 | hero]
  getModifiersFor _ _ _ = pure []

instance RunMessage CombatSpecialist where
  runMessage msg a@(CombatSpecialist attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> a <$ drawCards you 1
    _ -> CombatSpecialist <$> runMessage msg attrs
