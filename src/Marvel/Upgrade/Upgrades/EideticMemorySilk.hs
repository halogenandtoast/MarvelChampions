module Marvel.Upgrade.Upgrades.EideticMemorySilk where
import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Upgrade.Types

eideticMemorySilk :: UpgradeCard EideticMemorySilk
eideticMemorySilk = upgrade EideticMemorySilk Cards.eideticMemorySilk
newtype EideticMemorySilk = EideticMemorySilk (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage EideticMemorySilk where
  runMessage msg u@(EideticMemorySilk attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      let revealing = [card | W.EncounterCardRevealed _ _ card <- windows]
      tucked <- tuckedEncounterCards you
      for_ (headMay revealing) $ \incoming -> do
        let matching = [card | card <- tucked, isJust (cdEncounterSet $ getCardDef card), cdEncounterSet (getCardDef card) == cdEncounterSet (getCardDef incoming)]
        chooseOne you [targeting card (swapRevealingTuckedCard you incoming card) | card <- matching]
      pure u
    _ -> EideticMemorySilk <$> runMessage msg attrs

instance HasAbilities EideticMemorySilk where
  getAbilities u@(EideticMemorySilk _attrs) = [limitedWindowAbility u 1 (W.EncounterCardRevealedMatchingTucked W.Would You) Interrupt OwnsThis ExhaustCost (runAbility u 1)]
