module Marvel.Event.Events.LeadFromTheFront (
  leadFromTheFront,
  leadFromTheFrontEffect,
  LeadFromTheFrontEffect (..),
  LeadFromTheFront (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Effect.Types
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Ref

leadFromTheFront :: EventCard LeadFromTheFront
leadFromTheFront = event LeadFromTheFront Cards.leadFromTheFront

newtype LeadFromTheFront = LeadFromTheFront (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage LeadFromTheFront where
  runMessage msg e@(LeadFromTheFront attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        players <- getPlayers
        chooseOne
          identityId
          [ TargetLabel
            (toRef targetIdentityId)
            [ Run $
                map
                  (CreatedEffect Cards.leadFromTheFront $ toSource e)
                  [ IdentityEntity $ IdentityWithId targetIdentityId
                  , AllyEntity $ AllyControlledBy $ IdentityWithId targetIdentityId
                  ]
            ]
          | targetIdentityId <- players
          ]
        pure e
      _ -> LeadFromTheFront <$> runMessage msg attrs
    _ -> LeadFromTheFront <$> runMessage msg attrs

newtype LeadFromTheFrontEffect = LeadFromTheFrontEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

leadFromTheFrontEffect :: CardEffect LeadFromTheFrontEffect
leadFromTheFrontEffect =
  effectWith
    LeadFromTheFrontEffect
    Cards.leadFromTheFront
    (modifiersL .~ [ThwartModifier 1, AttackModifier 1])

instance RunMessage LeadFromTheFrontEffect where
  runMessage msg e@(LeadFromTheFrontEffect attrs) = case msg of
    EndPhase _ -> e <$ push (EffectMessage (effectId attrs) DisableEffect)
    _ -> LeadFromTheFrontEffect <$> runMessage msg attrs
