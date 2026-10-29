module Marvel.Event.Events.SmashTheProblem (smashTheProblem, SmashTheProblem (..)) where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Count

smashTheProblem :: EventCard SmashTheProblem
smashTheProblem = event SmashTheProblem Cards.smashTheProblem

newtype SmashTheProblem = SmashTheProblem (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent SmashTheProblem where
  eventTargets (SmashTheProblem attrs) = do
    let you = eventController attrs
    confused <- identityMatches ConfusedIdentity you
    atk <- selectCount HeroAttackDamage (IdentityWithId you)
    pure $ if confused || atk > 0 then Nothing else Just []

instance RunMessage SmashTheProblem where
  runMessage msg e@(SmashTheProblem attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      atk <- selectCount HeroAttackDamage (IdentityWithId you)
      removeThreat you attrs atk SchemeYouCanThwart
      pure e
    _ -> SmashTheProblem <$> runMessage msg attrs
