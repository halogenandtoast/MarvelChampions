module Marvel.Upgrade.Upgrades.SpiderTingle where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Question (runAbility)
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Card
import Marvel.Card.Id
import Marvel.Id (TreacheryId (..))
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Trait
import Marvel.Window qualified as W

spiderTingle :: UpgradeCard SpiderTingle
spiderTingle = upgrade SpiderTingle Cards.spiderTingle

newtype SpiderTingle = SpiderTingle (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SpiderTingle where
  getAbilities a = [limitedWindowAbility a 1 (W.EncounterCardRevealedBy W.Would You AnyEncounterCard)
    Interrupt OwnsThis (DamageCharacterCost (CharacterWithTrait WebWarrior <> CharacterControlledBy You) 1) (runAbility a 1)]

instance RunMessage SpiderTingle where
  runMessage msg u@(SpiderTingle attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [card | W.EncounterCardRevealed _ _ card <- windows, cdCardType (getCardDef card) == TreacheryType] $ \card -> do
        cancelEncounterWhenRevealed card
        removeFromPlay attrs
      pure u
    _ -> SpiderTingle <$> runMessage msg attrs
