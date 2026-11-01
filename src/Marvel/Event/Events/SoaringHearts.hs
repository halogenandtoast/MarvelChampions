module Marvel.Event.Events.SoaringHearts where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability.Types qualified as Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Name (title)
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

soaringHearts :: EventCard SoaringHearts
soaringHearts = event SoaringHearts Cards.soaringHearts

newtype SoaringHearts = SoaringHearts (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage SoaringHearts where
  runMessage msg e@(SoaringHearts a) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId a -> do
      cards <- selectList (InDiscardOf (IdentityWithId you) $ BasicCardMatches $ HeroSpecificCard <> CardWithType EventType)
      chooseOne you [targeting card $ addToHand you card | card <- cards]
      cardSpecific a "ready" ()
      pure e
    CardSpecific (isTarget a -> True) "ready" _ -> do
      characters <- selectList AnyCharacter
      for_ characters $ \character -> do
        def <- getsGame (`abilityCardDef` characterRef character)
        when (maybe False (\d -> title (cdName d) `elem` ["Angel", "Psylocke"]) def) $ case character of
          IdentityCharacter you -> ready you
          AllyCharacter aid -> ready aid
          _ -> pure ()
      pure e
    _ -> SoaringHearts <$> runMessage msg a
