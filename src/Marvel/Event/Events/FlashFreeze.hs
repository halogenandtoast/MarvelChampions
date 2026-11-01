module Marvel.Event.Events.FlashFreeze where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Effect.Types
import Marvel.Entity ()
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

flashFreeze :: EventCard FlashFreeze
flashFreeze = event FlashFreeze Cards.flashFreeze

newtype FlashFreeze = FlashFreeze (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage FlashFreeze where
  runMessage msg e@(FlashFreeze attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      createEffect Cards.flashFreeze attrs (VillainsAndEngagedMinions you)
      weather <- selectList (SupportNamed "Blizzard")
      for_ weather (resolveWeatherSpecial you)
      pure e
    _ -> FlashFreeze <$> runMessage msg attrs

newtype FlashFreezeEffect = FlashFreezeEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

flashFreezeEffect :: CardEffect FlashFreezeEffect
flashFreezeEffect = initialize <$> effectWith FlashFreezeEffect Cards.flashFreeze (endsL ?~ DisableAtEndOfPhase)
 where
  initialize (FlashFreezeEffect attrs) = FlashFreezeEffect attrs
    { effectModifiers = case effectMatcher attrs of
        VillainsAndEngagedMinions you -> [AttackReductionWhileAttacking you 3]
        _ -> []
    }

instance RunMessage FlashFreezeEffect where
  runMessage msg (FlashFreezeEffect attrs) = FlashFreezeEffect <$> runMessage msg attrs
