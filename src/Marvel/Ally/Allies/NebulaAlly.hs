module Marvel.Ally.Allies.NebulaAlly (
  nebulaAlly,
  NebulaAlly (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Trait qualified as Trait
import Marvel.Window

nebulaAlly :: AllyCard NebulaAlly
nebulaAlly = ally NebulaAlly Cards.nebulaAlly (Thw 2, 1) (Atk 2, 1) (HP 2)

newtype NebulaAlly = NebulaAlly (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities NebulaAlly where
  getAbilities a =
    [limitedWindowAbility a 1 EntersPlayThis Response OwnsThis NoCost $ runAbility a 1]

-- | After she enters play, search your deck for an Attack or Thwart event and add it to
-- your hand. Shuffle your deck.
instance RunMessage NebulaAlly where
  runMessage msg a@(NebulaAlly attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      search
        ident
        (SearchIdentityDeck ident AllOfDeck)
        (CardWithType EventType <> CardMatchesAny [CardWithTrait Trait.Attack, CardWithTrait Trait.Thwart])
        SearchDrawOne
        ShuffleBackIn
      pure a
    _ -> NebulaAlly <$> runMessage msg attrs
