module Marvel.Event.Events.SoulSisters where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

soulSisters :: EventCard SoulSisters
soulSisters = event SoulSisters Cards.soulSisters

newtype SoulSisters = SoulSisters (Attrs Event)
  deriving anyclass (HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent SoulSisters where
  eventTargets _ = do
    characters <- concatMapM (\title -> do
      identities <- selectList (IdentityWithTitle title)
      allies <- selectList (AllyWithTitle title)
      pure $ map IdentityCharacter identities <> map AllyCharacter allies) ["Phoenix", "Storm"]
    changes <- anyM (\character -> do
      readyable <- characterMatches CharacterCanBeReadied character
      damage <- characterStoredDamage character
      modifiers <- getModifiers (characterRef character)
      pure $ readyable || (damage > 0 && CannotBeHealedByPlayerCards `notElem` modifiers)) characters
    pure $ if changes then Nothing else Just []

instance RunMessage SoulSisters where
  runMessage msg e@(SoulSisters attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _ _ _) | eid == eventId attrs -> do
      for_ ["Phoenix", "Storm"] \title -> do
        identities <- selectList (IdentityWithTitle title)
        allies <- selectList (AllyWithTitle title)
        for_ identities (\you -> ready you >> heal you 2)
        for_ allies (\aid -> ready aid >> heal aid 2)
      pure e
    _ -> SoulSisters <$> runMessage msg attrs
