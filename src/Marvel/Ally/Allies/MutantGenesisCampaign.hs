module Marvel.Ally.Allies.MutantGenesisCampaign where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Ref
import Marvel.Trait (Trait (Sentinel))
import Marvel.Window qualified as W

mgCampaignMagneto :: AllyCard MutantGenesisCampaignMagneto
mgCampaignMagneto = ally MutantGenesisCampaignMagneto Cards.mgCampaignMagneto (Thw 2,1) (Atk 3,1) (HP 5)

newtype MutantGenesisCampaignMagneto = MutantGenesisCampaignMagneto (Attrs Ally)
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor MutantGenesisCampaignMagneto where
  getModifiersFor _ target (MutantGenesisCampaignMagneto attrs)
    | target == toRef attrs = pure [ControlledByFirstPlayer,NotCountedAgainstAllyLimit]
  getModifiersFor _ _ _ = pure []

instance HasAbilities MutantGenesisCampaignMagneto where
  getAbilities a@(MutantGenesisCampaignMagneto attrs) =
    [limitedWindowAbility a 1 (W.EnemyDefeated W.After (MinionEnemy <> EnemyWithTrait Sentinel) (AttackFromAlly $ AllyWithId $ allyId attrs))
      Response OwnsThis NoCost (runAbility a 1)]

instance RunMessage MutantGenesisCampaignMagneto where
  runMessage msg a@(MutantGenesisCampaignMagneto attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> a <$ heal (allyId attrs) 1
    _ -> MutantGenesisCampaignMagneto <$> runMessage msg attrs
