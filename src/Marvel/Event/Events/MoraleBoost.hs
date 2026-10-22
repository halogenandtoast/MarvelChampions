module Marvel.Event.Events.MoraleBoost (
  moraleBoost,
  moraleBoostEffect,
  MoraleBoost (..),
  MoraleBoostEffect (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Effect.Types
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref

moraleBoost :: EventCard MoraleBoost
moraleBoost = event MoraleBoost Cards.moraleBoost

newtype MoraleBoost = MoraleBoost (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage MoraleBoost where
  runMessage msg e@(MoraleBoost attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        pushChoice identityId $
          CreateEffect Cards.moraleBoost (toSource attrs) (TargetMatches $ IdentityEntity HeroIdentity)
        pure e
      _ -> MoraleBoost <$> runMessage msg attrs
    _ -> MoraleBoost <$> runMessage msg attrs

newtype MoraleBoostEffect = MoraleBoostEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

moraleBoostEffect :: CardEffect MoraleBoostEffect
moraleBoostEffect =
  effectWith MoraleBoostEffect Cards.moraleBoost $
    (modifiersL .~ [ThwartModifier 1, AttackModifier 1, DefenseModifier 1])
      . (endsL ?~ DisableAtEndOfRound)

instance RunMessage MoraleBoostEffect where
  runMessage msg (MoraleBoostEffect attrs) = MoraleBoostEffect <$> runMessage msg attrs
