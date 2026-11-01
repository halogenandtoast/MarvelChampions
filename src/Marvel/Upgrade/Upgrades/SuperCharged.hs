module Marvel.Upgrade.Upgrades.SuperCharged where

import Marvel.Prelude
import Marvel.Resource (Resource (..))
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Payment
import Marvel.Question
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

superCharged :: UpgradeCard SuperCharged
superCharged = upgrade SuperCharged Cards.superCharged

newtype SuperCharged = SuperCharged (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SuperCharged where
  getAbilities a@(SuperCharged attrs) =
    [ability a 1 Action OwnsThis (DiscardMatchingHandCardCost $ CardWithType ResourceType <> CardMatchesAny [CardWithResource icon | icon <- [Energy, Mental, Physical, Wild]]) (runAbility a 1)] <>
    [limitedWindowAbility a 2 (W.MakesBasicAttack W.When $ IdentityWithId $ upgradeController attrs)
      HeroInterrupt OwnsThis (DiscardCost $ toTarget attrs) (runAbility a 2) | upgradeUses attrs > 0]

instance RunMessage SuperCharged where
  runMessage msg a@(SuperCharged attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ payment -> do
      addUpgradeUses (upgradeId attrs) $ fromIntegral $ sum $ map (length . printedResources . getCardDef) $ paymentDiscards payment
      pure a
    RanAbility you (isTarget attrs -> True) 2 _ _ -> do
      addToBasicPower you (min 8 $ upgradeUses attrs * 2)
      pure a
    _ -> SuperCharged <$> runMessage msg attrs
