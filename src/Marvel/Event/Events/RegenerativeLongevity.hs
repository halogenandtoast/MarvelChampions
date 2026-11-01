module Marvel.Event.Events.RegenerativeLongevity where

import Marvel.Prelude
import Marvel.Game.Source ()
import Marvel.Id
import Marvel.Query
import Marvel.Question ()
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

regenerativeLongevity :: EventCard RegenerativeLongevity
regenerativeLongevity = event RegenerativeLongevity Cards.regenerativeLongevity

newtype RegenerativeLongevity = RegenerativeLongevity (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage RegenerativeLongevity where
  runMessage msg e@(RegenerativeLongevity attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      cardSpecific attrs "heal" (you, 4 :: Natural)
      pure e
    CardSpecific (isTarget attrs -> True) "heal" (fromJSON -> Success (you, remaining :: Natural)) -> do
      when (remaining > 0) do
        gabby <- selectList (AllyWithTitle "Honey Badger")
        damaged <- selectList (CharacterWithAnyDamage <> CharacterCanBeHealed)
        let allowed = IdentityCharacter you : map AllyCharacter gabby
            characters = filter (`elem` allowed) damaged
        unless (null characters) $ chooseOne you
          [targeting (characterRef character) (heal character 1 >> cardSpecific attrs "heal" (you, remaining - 1)) | character <- characters]
      pure e
    _ -> RegenerativeLongevity <$> runMessage msg attrs
