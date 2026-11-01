module Marvel.Event.Events.FlyingFormation where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Trait qualified as Trait
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

flyingFormation :: EventCard FlyingFormation
flyingFormation = event FlyingFormation Cards.flyingFormation
newtype FlyingFormation = FlyingFormation (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage FlyingFormation where
  runMessage msg e@(FlyingFormation attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      cardSpecific attrs "ready" (you, 3 :: Natural, [] :: [CharacterId])
      pure e
    CardSpecific (isTarget attrs -> True) "ready" (fromJSON -> Success (you, remaining :: Natural, selected :: [CharacterId])) -> do
      when (remaining > 0) do
        characters <- filter (`notElem` selected) <$> selectList (CharacterControlledBy AnyIdentity <> CharacterWithTrait Trait.Aerial <> CharacterCanBeReadied)
        unless (null characters) $ chooseOne you $
          [targeting (characterRef character) (readyOne character >> cardSpecific attrs "ready" (you, remaining - 1, character : selected)) | character <- characters]
          <> [labeled "$label.done" (pure ())]
      pure e
    _ -> FlyingFormation <$> runMessage msg attrs

readyOne :: ReverseQueue m => CharacterId -> m ()
readyOne = \case
  IdentityCharacter ident -> ready ident
  AllyCharacter aid -> ready aid
  _ -> pure ()
