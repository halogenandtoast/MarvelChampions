module Marvel.Ally.Allies.CyclopsScottSummers where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

cyclopsScottSummers :: AllyCard CyclopsScottSummers
cyclopsScottSummers = ally CyclopsScottSummers Cards.cyclopsScottSummers (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype CyclopsScottSummers = CyclopsScottSummers (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities CyclopsScottSummers where
  getAbilities (CyclopsScottSummers a) =
    [ limitedWindowAbility a 1 W.EntersPlayThis Response (OwnsThis <> UpgradeExists (UpgradeNamed "Phoenix Force" <> upgradeControlledBy (allyController a))) NoCost (runAbility a 1)
    , limitedWindowAbility a 2 (W.AllyLeavesPlay W.When (IdentityWithId $ allyController a) (CardIs Cards.cyclopsScottSummers)) Response (OwnsThis <> UpgradeExists (UpgradeNamed "Phoenix Force" <> upgradeControlledBy (allyController a) <> UpgradeWithAnyUses)) NoCost (runAbility a 2)
    ]

instance RunMessage CyclopsScottSummers where
  runMessage msg e@(CyclopsScottSummers a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> e <$ addPhoenixPower you 2
    RanAbility you (isTarget a -> True) 2 _ _ -> e <$ removePhoenixPower you 2
    _ -> CyclopsScottSummers <$> runMessage msg a
