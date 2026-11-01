module Marvel.Support.Supports.AsteroidM where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Deck ()
import Marvel.Discard
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource.Types ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait qualified as T
import Marvel.Window ()
import Marvel.Identity.Types (Field(..))

import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

asteroidM :: SupportCard AsteroidM
asteroidM = support AsteroidM Cards.asteroidM

newtype AsteroidM = AsteroidM (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AsteroidM where
  getAbilities a = [ability a 1 AlterEgoAction OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage AsteroidM where
  runMessage msg e@(AsteroidM attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- filter (cardMatch $ CardWithTrait T.Magnetic) <$> projectMap PlayerIdentityDiscard unDiscard you
      for_ (headMay cards) (shuffleIntoDeck you . pure)
      heal you 1
      pure e
    _ -> AsteroidM <$> runMessage msg attrs
