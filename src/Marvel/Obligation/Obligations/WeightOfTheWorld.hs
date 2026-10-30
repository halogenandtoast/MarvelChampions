module Marvel.Obligation.Obligations.WeightOfTheWorld where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Question (runAbility)
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Upgrade.Cards qualified as Upgrades

weightOfTheWorld :: ObligationCard WeightOfTheWorld
weightOfTheWorld = obligation WeightOfTheWorld Cards.weightOfTheWorld

newtype WeightOfTheWorld = WeightOfTheWorld (Attrs Obligation)
  deriving anyclass (IsObligation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor WeightOfTheWorld where
  getModifiersFor _ (UpgradeRef uid) _ = do
    helmet <- upgradeMatches (UpgradeIs Upgrades.supernovaHelmet) uid
    pure [CannotReady | helmet]
  getModifiersFor _ _ _ = pure []

instance HasAbilities WeightOfTheWorld where
  getAbilities (WeightOfTheWorld a) = case obligationOwner a of
    Just _ -> [ability a 1 AlterEgoAction NoCriteria ExhaustCost (runAbility a 1)]
    Nothing -> []

instance RunMessage WeightOfTheWorld where
  runMessage msg o@(WeightOfTheWorld attrs) = runQueueT $ case msg of
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId attrs -> pure o
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> o <$ removeFromGame attrs
    _ -> WeightOfTheWorld <$> runMessage msg attrs
