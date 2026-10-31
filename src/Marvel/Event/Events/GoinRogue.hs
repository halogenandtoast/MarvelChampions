module Marvel.Event.Events.GoinRogue where

import Marvel.Prelude
import Marvel.Id
import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Keyword
import Marvel.Trait qualified as Trait

goinRogue :: EventCard GoinRogue
goinRogue = event GoinRogue Cards.goinRogue

newtype GoinRogue = GoinRogue (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage GoinRogue where
  runMessage msg e@(GoinRogue attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      aerial <- member Trait.Aerial <$> getCharacterTraits (IdentityCharacter you)
      removeThreat you attrs (if aerial then 5 else 3) SchemeYouCanThwart
      cardSpecific attrs "goinRogueBonuses" you
      pure e
    CardSpecific (isTarget attrs -> True) "goinRogueBonuses" (fromJSON -> Success you) -> do
      printed <- getsGame (fmap (toList . cdKeywords) . (`abilityCardDef` IdentityRef you))
      modifiers <- getModifiers (IdentityRef you)
      let keywords = fromMaybe [] printed <> [keyword | KeywordModifier keyword <- modifiers]
      when (any (\case Retaliate n -> n > 0; _ -> False) keywords) do
        enemies <- selectList EnemyCanBeConfused
        chooseOne you [targeting (enemyRef enemy) (confuse enemy attrs) | enemy <- enemies]
      when (Stalwart `elem` keywords) $ drawCards you 1
      pure e
    _ -> GoinRogue <$> runMessage msg attrs
