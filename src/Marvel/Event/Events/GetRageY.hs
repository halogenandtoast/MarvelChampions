module Marvel.Event.Events.GetRageY where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Effect.Types
import Marvel.Window qualified as W ()

getRageY :: EventCard GetRageY
getRageY = event GetRageY Cards.getRageY

newtype GetRageY = GetRageY (Attrs Event)
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage GetRageY where
  runMessage msg e@(GetRageY attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      allies <- selectList AnyAlly
      chooseOne you [targeting aid do
        ready aid
        createEffect Cards.getRageY attrs (AllyEntity $ AllyWithId aid)
        | aid <- allies]
      pure e
    _ -> GetRageY <$> runMessage msg attrs

newtype GetRageYEffect = GetRageYEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

getRageYEffect :: CardEffect GetRageYEffect
getRageYEffect = effectWith GetRageYEffect Cards.getRageY $
  (modifiersL .~ [AttackModifier 1]) . (endsL ?~ DisableAtEndOfPhase)

instance RunMessage GetRageYEffect where
  runMessage msg (GetRageYEffect attrs) = GetRageYEffect <$> runMessage msg attrs
