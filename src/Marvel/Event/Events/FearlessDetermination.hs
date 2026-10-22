module Marvel.Event.Events.FearlessDetermination (
  fearlessDetermination,
  fearlessDeterminationEffect,
  FearlessDeterminationEffect (..),
  FearlessDetermination (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Effect.Types
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Queue
import Marvel.Ref

fearlessDetermination :: EventCard FearlessDetermination
fearlessDetermination = event FearlessDetermination Cards.fearlessDetermination

newtype FearlessDetermination = FearlessDetermination (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage FearlessDetermination where
  runMessage msg e@(FearlessDetermination attrs) = case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      pushAll
        [ CreatedEffect Cards.fearlessDetermination (toSource e) (IdentityEntity $ IdentityWithId identityId)
        , IdentityMessage identityId $ DrawCards FromDeck 1
        ]
      pure e
    _ -> FearlessDetermination <$> runMessage msg attrs

-- | +1 THW until the end of the phase.
newtype FearlessDeterminationEffect = FearlessDeterminationEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

fearlessDeterminationEffect :: CardEffect FearlessDeterminationEffect
fearlessDeterminationEffect =
  effectWith FearlessDeterminationEffect Cards.fearlessDetermination $
    (modifiersL .~ [ThwartModifier 1]) . (endsL ?~ DisableAtEndOfPhase)

instance RunMessage FearlessDeterminationEffect where
  runMessage msg (FearlessDeterminationEffect attrs) = FearlessDeterminationEffect <$> runMessage msg attrs
