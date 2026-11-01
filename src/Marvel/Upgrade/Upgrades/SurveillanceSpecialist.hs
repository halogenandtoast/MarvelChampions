module Marvel.Upgrade.Upgrades.SurveillanceSpecialist where

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

surveillanceSpecialist :: UpgradeCard SurveillanceSpecialist
surveillanceSpecialist = upgrade SurveillanceSpecialist Cards.surveillanceSpecialist

newtype SurveillanceSpecialist = SurveillanceSpecialist (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SurveillanceSpecialist where
  getAbilities a = [limitedWindowAbility a 1 (W.MakesBasicThwart W.After You) HeroResponse OwnsThis ExhaustCost (runAbility a 1)]

instance HasModifiersFor SurveillanceSpecialist where
  getModifiersFor _ (IdentityRef you) (SurveillanceSpecialist attrs) | you == upgradeController attrs = do
    hero <- identityMatches HeroIdentity you
    pure [ThwartModifier 1 | hero]
  getModifiersFor _ _ _ = pure []

instance RunMessage SurveillanceSpecialist where
  runMessage msg a@(SurveillanceSpecialist attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> a <$ drawCards you 1
    _ -> SurveillanceSpecialist <$> runMessage msg attrs
