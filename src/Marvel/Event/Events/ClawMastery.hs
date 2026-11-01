module Marvel.Event.Events.ClawMastery where

import Marvel.Prelude
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Effect.Types
import Marvel.Keyword
import Marvel.Hero.Cards qualified as Heroes

clawMastery :: EventCard ClawMastery
clawMastery = event ClawMastery Cards.clawMastery

newtype ClawMastery = ClawMastery (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage ClawMastery where
  runMessage msg e@(ClawMastery attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _ _ _) | eid == eventId attrs -> do
      createEffect Cards.clawMastery attrs (IdentityEntity $ IdentityIs Heroes.x23 <> HeroIdentity)
      pure e
    _ -> ClawMastery <$> runMessage msg attrs

newtype ClawMasteryEffect = ClawMasteryEffect (Attrs Effect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance IsEffect ClawMasteryEffect where
  toEffectAttrs (ClawMasteryEffect attrs) = attrs
  getEffectModifiers _ = do
    gabby <- selectAny (AllyWithTitle "Honey Badger")
    pure $ [AttackModifier 2] <> [KeywordModifier Overkill | gabby]

clawMasteryEffect :: CardEffect ClawMasteryEffect
clawMasteryEffect = effectWith ClawMasteryEffect Cards.clawMastery (endsL ?~ DisableAtEndOfRound)

instance RunMessage ClawMasteryEffect where
  runMessage msg (ClawMasteryEffect attrs) = runQueueT $ ClawMasteryEffect <$> runMessage msg attrs
