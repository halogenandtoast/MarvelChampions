module Marvel.Event.Events.ProblemSolvers (problemSolvers, ProblemSolvers (..)) where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Id
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Count
import Marvel.Game.Source (HasGame)
import Marvel.Payment
import Marvel.Trait

problemSolvers :: EventCard ProblemSolvers
problemSolvers = event ProblemSolvers Cards.problemSolvers

newtype ProblemSolvers = ProblemSolvers (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

characterThwart :: HasGame m => CharacterId -> m Natural
characterThwart = \case
  IdentityCharacter ident -> selectCount HeroThwartValue (IdentityWithId ident)
  AllyCharacter aid -> selectCount AllyThwartValue (AllyWithId aid)
  _ -> pure 0

instance IsEvent ProblemSolvers where
  eventExhaustionCostValid (ProblemSolvers attrs) characters = do
    confused <- identityMatches ConfusedIdentity (eventController attrs)
    total <- sum <$> traverse characterThwart characters
    pure $ confused || total > 0
  eventTargets (ProblemSolvers attrs) = do
    avengers <- filterM unexhausted =<< selectList (CharacterControlledBy AnyIdentity <> CharacterWithTrait Avenger)
    guardians <- filterM unexhausted =<< selectList (CharacterControlledBy AnyIdentity <> CharacterWithTrait Guardian)
    confused <- identityMatches ConfusedIdentity (eventController attrs)
    changes <- anyM (\(a, g) -> (> 0) <$> ((+) <$> characterThwart a <*> characterThwart g))
      [(a, g) | a <- avengers, g <- guardians, a /= g]
    pure $ if confused || changes then Nothing else Just []
   where
    unexhausted = \case
      IdentityCharacter ident -> identityMatches UnexhaustedIdentity ident
      AllyCharacter aid -> selectAny (AllyWithId aid <> UnexhaustedAlly)
      _ -> pure False

instance RunMessage ProblemSolvers where
  runMessage msg e@(ProblemSolvers attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _ payment _) | eid == eventId attrs -> do
      thw <- sum <$> traverse characterThwart (paymentExhaustedCharacters payment)
      schemes <- selectList SchemeYouCanThwart
      thwartEach attrs [(scheme, thw) | scheme <- schemes]
      pure e
    _ -> ProblemSolvers <$> runMessage msg attrs
