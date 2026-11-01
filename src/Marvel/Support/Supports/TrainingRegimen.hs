module Marvel.Support.Supports.TrainingRegimen where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Id
import Marvel.Matchers
import Marvel.Query
import Marvel.Question
import Marvel.Payment
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Keyword qualified as Keyword
import Marvel.Card
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

trainingRegimen :: SupportCard TrainingRegimen
trainingRegimen = support TrainingRegimen Cards.trainingRegimen

newtype TrainingRegimen = TrainingRegimen (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities TrainingRegimen where
  getAbilities (TrainingRegimen a) = [ability a 1 Action OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage TrainingRegimen where
  runMessage msg e@(TrainingRegimen a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      search you (SearchIdentityDeck you AllOfDeck) (CardWithTrait Trait.Skill) SearchDrawOne ShuffleBackIn
      cardSpecific a "discardIfHero" you
      pure e
    CardSpecific (isTarget a -> True) "discardIfHero" (fromJSON -> Success you) -> do
      heroForm <- identityMatches HeroIdentity you
      when heroForm $ discardFromHand you 1
      pure e
    _ -> TrainingRegimen <$> runMessage msg a
