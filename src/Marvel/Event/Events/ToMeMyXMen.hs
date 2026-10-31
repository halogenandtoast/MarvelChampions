module Marvel.Event.Events.ToMeMyXMen where

import Marvel.Prelude
import Marvel.Card
import Marvel.Effect.Types
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait qualified as Trait

-- The search keeps its top five cards separate until the chosen ally enters play.
toMeMyXMen :: EventCard ToMeMyXMen
toMeMyXMen = event ToMeMyXMen Cards.toMeMyXMen

newtype ToMeMyXMen = ToMeMyXMen (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage ToMeMyXMen where
  runMessage msg e@(ToMeMyXMen attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      legal <- selectList (CanEnterPlay <> BasicCardMatches (CardWithType AllyType <> CardWithTrait Trait.XMen))
      search you (SearchIdentityDeck you $ TopOfDeck 5)
        (CardMatchesAny $ map (CardIs . getCardDef) legal) (SearchTarget $ toTarget attrs) ShuffleBackIn
      pure e
    SearchFoundCards (isTarget attrs -> True) (onlyPlayerCards -> cards) -> do
      for_ cards \card -> do
        let aid = AllyId $ unCardId $ pcCardId card
        createEffect Cards.toMeMyXMen aid (AllyEntity $ AllyWithId aid)
        putIntoPlay (eventController attrs) card
      pure e
    _ -> ToMeMyXMen <$> runMessage msg attrs

newtype ToMeMyXMenEffect = ToMeMyXMenEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

toMeMyXMenEffect :: CardEffect ToMeMyXMenEffect
toMeMyXMenEffect = effect ToMeMyXMenEffect Cards.toMeMyXMen

instance RunMessage ToMeMyXMenEffect where
  runMessage msg e@(ToMeMyXMenEffect attrs) = runQueueT $ case msg of
    EndPhase _ -> do
      for_ tracked \aid -> do
        whenM (selectAny $ AllyWithId aid) $ returnToHand aid
        let mid = MinionId $ unAllyId aid
        whenM (selectAny $ MinionWithId mid) $ returnToHand mid
      disableEffect (effectId attrs)
      pure e
    RemoveFromPlay target | follows target -> e <$ disableEffect (effectId attrs)
    RemoveFromGame target | follows target -> e <$ disableEffect (effectId attrs)
    ReturnToHand target | follows target -> e <$ disableEffect (effectId attrs)
    _ -> ToMeMyXMenEffect <$> runMessage msg attrs
   where
    tracked = case effectSource attrs of AllyRef aid -> Just aid; _ -> Nothing
    follows target = any (\aid -> case target of
      AllyRef a -> a == aid
      MinionRef m -> unMinionId m == unAllyId aid
      CardIdRef c -> unCardId c == unAllyId aid
      CharacterRef (AllyCharacter a) -> a == aid
      EnemyRef (EnemyMinionId m) -> unMinionId m == unAllyId aid
      _ -> False) tracked
