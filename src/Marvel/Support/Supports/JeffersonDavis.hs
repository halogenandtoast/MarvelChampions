module Marvel.Support.Supports.JeffersonDavis where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Count (selectCount)
import Marvel.GameValue
import Marvel.Matchers.Types (GameValueMatcher (GreaterThan))
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Query
import Marvel.Question (runAbility)

jeffersonDavis :: SupportCard JeffersonDavis
jeffersonDavis = support JeffersonDavis Cards.jeffersonDavis

newtype JeffersonDavis = JeffersonDavis (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities JeffersonDavis where
  getAbilities a = [ability a 1 AlterEgoAction (OwnsThis <> SchemeExists (SchemeWithLeastThreat <> SchemeWithThreat (GreaterThan $ Static 0))) ExhaustCost (runAbility a 1)]

instance RunMessage JeffersonDavis where
  runMessage msg s@(JeffersonDavis attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      schemes <- selectList AnyScheme
      scored <- for schemes \sid -> (sid,) <$> selectCount SchemeThreat (SchemeWithId sid)
      chooseOne you [targeting (schemeRef sid) (thwartScheme sid attrs 1) | sid <- extremes LeastThreat scored]
      pure s
    _ -> JeffersonDavis <$> runMessage msg attrs
