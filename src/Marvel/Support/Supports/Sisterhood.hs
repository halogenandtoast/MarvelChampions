module Marvel.Support.Supports.Sisterhood where

import Marvel.Prelude
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Question (runAbility)
import Marvel.Ally.Cards qualified as Allies
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

sisterhood :: SupportCard Sisterhood
sisterhood = support Sisterhood Cards.sisterhood

newtype Sisterhood = Sisterhood (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

x23Cards :: CardMatcher
x23Cards = CardMatchesAny $ map CardWithTitle
  ["X-23's Claws", "Honey Badger", "Animal Instinct", "Claw Mastery", "Regenerative Longevity", "Sisterly Bond", "Sisterhood", "Adamantium Lacing", "Grim Resolve", "Pain Tolerance", "Puncture Wound"]

instance HasAbilities Sisterhood where
  getAbilities a = [ability a 1 Action OwnsThis (ExhaustCost <> DiscardMatchingHandCardCost x23Cards) (runAbility a 1)]

instance RunMessage Sisterhood where
  runMessage msg a@(Sisterhood attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      search you (SearchIdentityDeckAndDiscardPile you) (CardIs Allies.honeyBadger) SearchDrawOne ShuffleBackIn
      pure a
    _ -> Sisterhood <$> runMessage msg attrs
