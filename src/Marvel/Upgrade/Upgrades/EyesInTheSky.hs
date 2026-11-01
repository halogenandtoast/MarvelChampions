module Marvel.Upgrade.Upgrades.EyesInTheSky where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
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
import Marvel.Trait qualified as Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

eyesInTheSky :: UpgradeCard EyesInTheSky
eyesInTheSky = upgrade EyesInTheSky Cards.eyesInTheSky
newtype EyesInTheSky = EyesInTheSky (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities EyesInTheSky where
  getAbilities a = [limitedWindowAbility a 1
    (W.EncounterCardRevealedBy W.Would You $ EncounterCardMatching $ CardWithType MinionType <> NotCardMatching (CardWithTrait Trait.Elite))
    HeroInterrupt (OwnsThis <> CanCancelRevealedEncounterCard)
    (ExhaustCharacterCost (CharacterControlledBy You <> CharacterWithTrait Trait.Aerial) <> DiscardCost (toTarget a)) (runAbility a 1)]

instance RunMessage EyesInTheSky where
  runMessage msg a@(EyesInTheSky attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ [card | W.EncounterCardRevealed _ _ card <- windows] $ \card -> do
        cancelEncounterCard card
        cancelMatchingMessage (== DiscardedCard (toCard card))
        discardCard card
        drawAndRevealEncounterCard you
      pure a
    _ -> EyesInTheSky <$> runMessage msg attrs
