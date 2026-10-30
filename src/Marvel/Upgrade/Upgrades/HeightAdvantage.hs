module Marvel.Upgrade.Upgrades.HeightAdvantage where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Question (runAbility)
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

heightAdvantage :: UpgradeCard HeightAdvantage
heightAdvantage = upgrade HeightAdvantage Cards.heightAdvantage

newtype HeightAdvantage = HeightAdvantage (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor HeightAdvantage where
  getModifiersFor _ (IdentityRef you) (HeightAdvantage attrs) | you == upgradeController attrs = do
    aerial <- identityMatches (IdentityWithTrait Aerial) you
    pure [AttackDamageReduction 1 | aerial]
  getModifiersFor _ _ _ = pure []

instance HasAbilities HeightAdvantage where
  getAbilities a@(HeightAdvantage attrs) =
    [limitedWindowAbility a 1 (W.TurnBeginsAt W.When $ IdentityWithId $ upgradeController attrs)
      ForcedInterrupt OwnsThis NoCost (runAbility a 1)]

instance RunMessage HeightAdvantage where
  runMessage msg a@(HeightAdvantage attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> a <$ discardUpgrade (upgradeId attrs)
    _ -> HeightAdvantage <$> runMessage msg attrs
