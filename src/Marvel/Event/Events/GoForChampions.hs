module Marvel.Event.Events.GoForChampions where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Effect.Types
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait

goForChampions :: EventCard GoForChampions
goForChampions = event GoForChampions Cards.goForChampions

newtype GoForChampions = GoForChampions (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage GoForChampions where
  runMessage msg e@(GoForChampions attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent _ _ _) | ident == eventId attrs -> do
      removeFromGame attrs
      createEffect Cards.goForChampions attrs (IdentityEntity $ IdentityWithTrait Champion)
      createEffect Cards.goForChampions attrs (AllyEntity $ AllyWithTrait Champion)
      pure e
    _ -> GoForChampions <$> runMessage msg attrs

newtype GoForChampionsEffect = GoForChampionsEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

goForChampionsEffect :: CardEffect GoForChampionsEffect
goForChampionsEffect = effectWith GoForChampionsEffect Cards.goForChampions
  ((modifiersL .~ [CannotTakeDamage]) . (endsL ?~ DisableAtEndOfRound))

instance RunMessage GoForChampionsEffect where
  runMessage msg (GoForChampionsEffect attrs) = GoForChampionsEffect <$> runMessage msg attrs
