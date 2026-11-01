module Marvel.Ally.Allies.SpiderManOttoOctavius where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Types
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Window qualified as W
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Query
import Marvel.Projection ()
import Marvel.Trait

spiderManOttoOctavius :: AllyCard SpiderManOttoOctavius
spiderManOttoOctavius = ally SpiderManOttoOctavius Cards.spiderManOttoOctavius (Thw 1, 1) (Atk 2, 2) (HP 2)

newtype SpiderManOttoOctavius = SpiderManOttoOctavius (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SpiderManOttoOctavius where
  getAbilities a = [limitedWindowAbility a 1 (W.PlayThis W.After) Response
    (OwnsThis <> UpgradeExists (UpgradeControlledBy You <> UpgradeCanBeReadied)) NoCost (runAbility a 1)]

instance RunMessage SpiderManOttoOctavius where
  runMessage msg a@(SpiderManOttoOctavius attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      upgrades <- selectList (UpgradeControlledBy (IdentityWithId you) <> UpgradeCanBeReadied)
      chooseOne you [targeting u do
        tech <- selectAny (UpgradeWithId u <> UpgradeWithTrait Tech)
        ready u
        when tech (drawCards you 1) | u <- upgrades]
      pure a
    _ -> SpiderManOttoOctavius <$> runMessage msg attrs
