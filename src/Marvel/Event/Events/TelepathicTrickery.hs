module Marvel.Event.Events.TelepathicTrickery where

import Data.List (nub)
import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

telepathicTrickery :: EventCard TelepathicTrickery
telepathicTrickery = event TelepathicTrickery Cards.telepathicTrickery

newtype TelepathicTrickery = TelepathicTrickery (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage TelepathicTrickery where
  runMessage msg e@(TelepathicTrickery a) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId a -> do
      schemes <- selectAny (ThwartableScheme <> SchemeWithoutThwartPrevention)
      when schemes $ removeThreat you a 4 ThwartableScheme
      cardSpecific a "statuses" you
      pure e
    CardSpecific (isTarget a -> True) "statuses" (fromJSON -> Success you) -> do
      unleashed <- identityMatches (IdentityWithTrait Trait.Unleashed) you
      when unleashed do
        stunnable <- selectList EnemyCanBeStunned
        confusable <- selectList EnemyCanBeConfused
        let enemies = nub (stunnable <> confusable)
        unless (null enemies) $ chooseOne you [targeting (enemyRef enemy) (stun enemy a >> confuse enemy a) | enemy <- enemies]
      pure e
    _ -> TelepathicTrickery <$> runMessage msg a
