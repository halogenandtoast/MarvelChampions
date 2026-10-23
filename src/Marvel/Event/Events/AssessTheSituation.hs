module Marvel.Event.Events.AssessTheSituation (
  assessTheSituation,
  AssessTheSituation (..),
  assessTheSituationEffect,
  AssessTheSituationEffect (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Effect.Types

assessTheSituation :: EventCard AssessTheSituation
assessTheSituation = event AssessTheSituation Cards.assessTheSituation

newtype AssessTheSituation = AssessTheSituation (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | You get +1 hand size until the end of the phase.
instance RunMessage AssessTheSituation where
  runMessage msg e@(AssessTheSituation attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      createEffect Cards.assessTheSituation attrs (IdentityEntity $ IdentityWithId you)
      pure e
    _ -> AssessTheSituation <$> runMessage msg attrs

newtype AssessTheSituationEffect = AssessTheSituationEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

assessTheSituationEffect :: CardEffect AssessTheSituationEffect
assessTheSituationEffect =
  effectWith AssessTheSituationEffect Cards.assessTheSituation $
    (modifiersL .~ [HandSizeModifier 1]) . (endsL ?~ DisableAtEndOfPhase)

instance RunMessage AssessTheSituationEffect where
  runMessage msg (AssessTheSituationEffect attrs) = AssessTheSituationEffect <$> runMessage msg attrs
