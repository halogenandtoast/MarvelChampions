module Marvel.Event.Events.SmoothasSilkSilk where
import Marvel.Prelude
import Marvel.Id
import Marvel.Query
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers ()

import Marvel.Ability ()
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Event.Types

smoothasSilkSilk :: EventCard SmoothasSilkSilk
smoothasSilkSilk = event SmoothasSilkSilk Cards.smoothasSilkSilk
newtype SmoothasSilkSilk = SmoothasSilkSilk (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage SmoothasSilkSilk where
  runMessage msg u@(SmoothasSilkSilk attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      enemies <- selectList AnyEnemy
      schemes <- selectList AnyScheme
      chooseOne you ([targeting (enemyRef enemy) (cardSpecific attrs "target" (you,enemyRef enemy)) | enemy <- enemies] <> [targeting (schemeRef scheme) (cardSpecific attrs "target" (you,schemeRef scheme)) | scheme <- schemes])
      pure u
    CardSpecific (isTarget attrs -> True) "target" (fromJSON -> Success (_you :: IdentityId,target :: Target)) -> do
      def <- getsGame (`abilityCardDef` target)
      for_ (cdEncounterSet =<< def) $ \set -> discardUntil FromEncounterDeck (CardInEncounterSet set) attrs
      pure u
    WithDiscardedMatch (isTarget attrs -> True) FromEncounterDeck (EncounterCard card) -> do
      tuckEncounterCard (eventController attrs) card
      pure u
    _ -> SmoothasSilkSilk <$> runMessage msg attrs
