module Marvel.Event.Events.CosmicAlliance (cosmicAlliance, CosmicAlliance (..)) where

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
import Marvel.Payment
import Marvel.Trait

cosmicAlliance :: EventCard CosmicAlliance
cosmicAlliance = event CosmicAlliance Cards.cosmicAlliance

newtype CosmicAlliance = CosmicAlliance (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent CosmicAlliance where
  eventTargets _ = do
    avengers <- selectList (CharacterControlledBy AnyIdentity <> CharacterWithTrait Avenger)
    guardians <- selectList (CharacterControlledBy AnyIdentity <> CharacterWithTrait Guardian)
    readyable <- selectList CharacterCanBeReadied
    let changes = or [a /= g && (a `elem` readyable && g `elem` readyable) | a <- avengers, g <- guardians]
    pure $ if changes then Nothing else Just []

instance RunMessage CosmicAlliance where
  runMessage msg e@(CosmicAlliance attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _ payment _) | eid == eventId attrs -> do
      for_ (paymentChosenCharacters payment) $ \case
        IdentityCharacter ident -> ready ident
        AllyCharacter aid -> ready aid
        _ -> pure ()
      pure e
    _ -> CosmicAlliance <$> runMessage msg attrs
