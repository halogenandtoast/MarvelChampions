module Marvel.Upgrade.Upgrades.LuckyandGood where
import Marvel.Prelude
import Marvel.Card
import Marvel.Game.Source
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Domino
import Marvel.Deck
import Marvel.Discard
import Marvel.Hand
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Resource.Types
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

luckyandGood :: UpgradeCard LuckyandGood
luckyandGood = upgrade LuckyandGood Cards.luckyandGood
newtype LuckyandGood = LuckyandGood (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities LuckyandGood where
  getAbilities a@(LuckyandGood attrs) = [subtype Defense $ limitedWindowAbility a 1
    (W.BoostCardFlipped W.When (EnemyAttackingCharacter $ CharacterControlledBy $ IdentityWithId $ upgradeController attrs)) HeroInterrupt OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage LuckyandGood where
  runMessage msg a@(LuckyandGood attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ [(card,enemy) | W.FlippedBoostCard card enemy <- windows] $ \(card,enemy) -> do
        discardBoostCardBeforeReveal card
        dealBoost (enemyRef enemy)
      pure a
    _ -> LuckyandGood <$> runMessage msg attrs
