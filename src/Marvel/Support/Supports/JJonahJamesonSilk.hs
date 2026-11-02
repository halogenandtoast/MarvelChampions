module Marvel.Support.Supports.JJonahJamesonSilk where
import Marvel.Prelude
import Marvel.GameValue (GameValue (Static))
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers ()

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Support.Types

jJonahJamesonSilk :: SupportCard JJonahJamesonSilk
jJonahJamesonSilk = support JJonahJamesonSilk Cards.jJonahJamesonSilk
newtype JJonahJamesonSilk = JJonahJamesonSilk (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage JJonahJamesonSilk where
  runMessage msg u@(JJonahJamesonSilk attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      search you (SearchIdentityDeckAndDiscardPile you) (CardIs Cards.gettheScoopSilk) (SearchTarget $ toRef attrs) ShuffleBackIn
      pure u
    SearchFoundCards (isTarget attrs -> True) (onlyPlayerCards -> cards) -> do
      let you = supportController attrs
      traverse_ (putIntoPlay you) cards
      pure u
    RanAbility you (isTarget attrs -> True) 2 _ _ -> u <$ removeThreat you attrs 2 (NotScheme MainScheme <> ThwartableScheme)
    _ -> JJonahJamesonSilk <$> runMessage msg attrs

instance HasAbilities JJonahJamesonSilk where
  getAbilities u@(JJonahJamesonSilk _attrs) = [ability u 1 AlterEgoAction OwnsThis ExhaustCost (runAbility u 1), ability u 2 Action (OwnsThis <> SchemeExists (SchemeWithThreat (GreaterThan $ Static 0) <> NotScheme MainScheme)) ExhaustCost (runAbility u 2)]
