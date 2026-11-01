module Marvel.Support.Supports.Cerebro where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W ()
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

cerebro :: SupportCard Cerebro
cerebro = support Cerebro Cards.cerebro

newtype Cerebro = Cerebro (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Cerebro where
  getAbilities a = [ability a 1 AlterEgoAction OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage Cerebro where
  runMessage msg e@(Cerebro attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      psionic <- selectAny (CharacterControlledBy (IdentityWithId you) <> CharacterWithTrait Trait.Psionic)
      search you (SearchIdentityDeck you $ if psionic then AllOfDeck else TopOfDeck 5)
        (CardWithType AllyType <> CardWithTrait Trait.XMen) SearchDrawOne ShuffleBackIn
      pure e
    _ -> Cerebro <$> runMessage msg attrs
