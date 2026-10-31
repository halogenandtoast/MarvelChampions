module Marvel.Ally.Allies.Forge where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

forge :: AllyCard Forge
forge = ally Forge Cards.forge (Thw 1, 1) (Atk 1, 1) (HP 2)

newtype Forge = Forge (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Forge where
  getAbilities a = [limitedWindowAbility a 1 W.EntersPlayThis Response OwnsThis NoCost (runAbility a 1)]

instance RunMessage Forge where
  runMessage msg a@(Forge attrs) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      search you (SearchIdentityDeckAndDiscardPile you)
        (CardWithType SupportType <> CardMatchesAny [CardWithTrait Trait.XMen, CardWithTrait Trait.XForce])
        SearchDrawOne ShuffleBackIn
      pure a
    _ -> Forge <$> runMessage msg attrs
