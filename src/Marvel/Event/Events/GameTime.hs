module Marvel.Event.Events.GameTime where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Id (CharacterId (AllyCharacter))
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Ref
import Marvel.Trait qualified as Trait

gameTime :: EventCard GameTime
gameTime = event GameTime Cards.gameTime

newtype GameTime = GameTime (Attrs Event)
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent GameTime where
  eventTargets _ = do
    allies <- selectList $ AllyWithUpgrade $ UpgradeWithTrait Trait.Training
    eligible <- filterM (\aid -> (||)
      <$> characterMatches CharacterCanBeReadied (AllyCharacter aid)
      <*> characterMatches CharacterWithAnyDamage (AllyCharacter aid)) allies
    pure $ Just $ map toTarget eligible

instance RunMessage GameTime where
  runMessage msg e@(GameTime attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _ payment _) | eid == eventId attrs -> do
      for_ (paymentTarget payment) \case
        AllyRef aid -> ready aid >> heal aid 1
        _ -> pure ()
      pure e
    _ -> GameTime <$> runMessage msg attrs
