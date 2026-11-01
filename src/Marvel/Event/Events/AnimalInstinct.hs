module Marvel.Event.Events.AnimalInstinct where

import Marvel.Prelude
import Marvel.Game.Source
import Marvel.Id
import Marvel.Query
import Marvel.Question
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Effect.Types
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity.Types (Field (PlayerIdentitySide, PlayerIdentitySides), PlayerIdentitySide (..))
import Marvel.Projection
import Marvel.Window qualified as W

animalInstinct :: EventCard AnimalInstinct
animalInstinct = event AnimalInstinct Cards.animalInstinct

newtype AnimalInstinct = AnimalInstinct (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage AnimalInstinct where
  runMessage msg e@(AnimalInstinct attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _ _ (Just (W.MadeBasicThwart you))) | eid == eventId attrs -> do
      side <- project PlayerIdentitySide you
      sides <- project PlayerIdentitySides you
      bonus <- case lookup side sides of
        Just (HeroSide h) -> Hero.getModifiedAttack (toAttrs h)
        _ -> pure 0
      createEffect Cards.animalInstinct attrs (IdentityEntity $ IdentityWithId you)
      cardSpecific attrs "animalInstinctBonus" bonus
      pure e
    _ -> AnimalInstinct <$> runMessage msg attrs

newtype AnimalInstinctEffect = AnimalInstinctEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

animalInstinctEffect :: CardEffect AnimalInstinctEffect
animalInstinctEffect = effect AnimalInstinctEffect Cards.animalInstinct

instance RunMessage AnimalInstinctEffect where
  runMessage msg e@(AnimalInstinctEffect attrs) = runQueueT $ case msg of
    CardSpecific source "animalInstinctBonus" (fromJSON -> Success (bonus :: Natural))
      | refocus source == effectSource attrs -> pure $ AnimalInstinctEffect $ attrs & modifiersL .~ [ThwartModifier bonus]
    CheckWindows ws | any ends ws -> e <$ disableEffect (effectId attrs)
    _ -> AnimalInstinctEffect <$> runMessage msg attrs
   where
    ends = \case
      W.Window W.After (W.MadeBasicThwart you) -> effectMatcher attrs == IdentityEntity (IdentityWithId you)
      _ -> False
