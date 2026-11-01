module Marvel.Event.Events.LuckBeaLady where
import Marvel.Prelude
import Marvel.Card
import Marvel.Game.Source
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Domino
import Marvel.Deck
import Marvel.Discard
import Marvel.Hand
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Resource.Types
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

luckBeaLady :: EventCard LuckBeaLady
luckBeaLady = event LuckBeaLady Cards.luckBeaLady
newtype LuckBeaLady = LuckBeaLady (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage LuckBeaLady where
  runMessage msg a@(LuckBeaLady attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> a <$ discardFromDeck you 1 attrs
    WithDiscarded (isTarget attrs -> True) FromDeck cards -> do
      resources <- discardedResources (eventController attrs) cards
      for_ resources $ cardSpecific attrs "resource"
      pure a
    CardSpecific (isTarget attrs -> True) "resource" (fromJSON -> Success resource) -> do
      case resource of
        Energy -> cardSpecific attrs "heal" ()
        Mental -> cardSpecific attrs "thwart" ()
        Physical -> cardSpecific attrs "damage" ()
        Wild -> do
          hurt <- selectList (CharacterWithDamage $ AtLeast $ Static 1)
          schemes <- selectList ThwartableScheme
          enemies <- selectList DamageableEnemy
          chooseOne (eventController attrs) $
            [labeled "$label.dominoHeal" (cardSpecific attrs "heal" ()) | notNull hurt]
            <> [labeled "$label.dominoThwart" (cardSpecific attrs "thwart" ()) | notNull schemes]
            <> [labeled "$label.dominoDamage" (cardSpecific attrs "damage" ()) | notNull enemies]
      pure a
    CardSpecific (isTarget attrs -> True) "heal" _ -> do
      hurt <- selectList (CharacterWithDamage $ AtLeast $ Static 1)
      chooseOne (eventController attrs) [targeting (characterRef c) (heal c 2) | c <- hurt]
      pure a
    CardSpecific (isTarget attrs -> True) "thwart" _ -> a <$ removeThreat (eventController attrs) attrs 2 ThwartableScheme
    CardSpecific (isTarget attrs -> True) "damage" _ -> a <$ chooseDamage (eventController attrs) attrs (toDamage 3 FromAbility) DamageableEnemy
    _ -> LuckBeaLady <$> runMessage msg attrs
