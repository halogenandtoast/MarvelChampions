module Marvel.Support.Supports.JessicaDrewsApartment (
  jessicaDrewsApartment,
  JessicaDrewsApartment (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Aspect
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

jessicaDrewsApartment :: SupportCard JessicaDrewsApartment
jessicaDrewsApartment = support JessicaDrewsApartment Cards.jessicaDrewsApartment

newtype JessicaDrewsApartment = JessicaDrewsApartment (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Alter-Ego Action: Exhaust Jessica Drew's Apartment → search the top 5 cards of your
-- deck for an aspect card and add it to your hand. Shuffle your deck.
instance HasAbilities JessicaDrewsApartment where
  getAbilities a = [ability a 1 AlterEgoAction OwnsThis ExhaustCost $ runAbility a 1]

instance RunMessage JessicaDrewsApartment where
  runMessage msg s@(JessicaDrewsApartment attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      search ident (SearchIdentityDeck ident $ TopOfDeck 5) (CardMatchesAny $ map CardWithAspect [Aggression, Justice, Leadership, Protection, Pool]) SearchDrawOne ShuffleBackIn
      pure s
    _ -> JessicaDrewsApartment <$> runMessage msg attrs
