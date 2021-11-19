module Marvel.Event.Events.LeadFromTheFront
  ( leadFromTheFront
  , leadFromTheFrontEffect
  , LeadFromTheFrontEffect(..)
  , LeadFromTheFront(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Effect.Attrs
import Marvel.Entity
import Marvel.Event.Attrs
import Marvel.Event.Cards qualified as Cards
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Target

leadFromTheFront :: EventCard LeadFromTheFront
leadFromTheFront = event LeadFromTheFront Cards.leadFromTheFront

newtype LeadFromTheFront = LeadFromTheFront EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage LeadFromTheFront where
  runMessage msg e@(LeadFromTheFront attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent identityId _ _ -> do
        players <- getPlayers
        e <$ chooseOne
          identityId
          [ TargetLabel
              (IdentityTarget ident)
              [ Run $ map
                  (CreatedEffect Cards.leadFromTheFront $ toSource e)
                  [ IdentityEntity $ IdentityWithId ident
                  , AllyEntity $ AllyControlledBy $ IdentityWithId ident
                  ]
              ]
          | ident <- players
          ]
      _ -> LeadFromTheFront <$> runMessage msg attrs
    _ -> LeadFromTheFront <$> runMessage msg attrs

newtype LeadFromTheFrontEffect = LeadFromTheFrontEffect EffectAttrs
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, IsSource, IsTarget)

leadFromTheFrontEffect :: CardEffect LeadFromTheFrontEffect
leadFromTheFrontEffect = effectWith
  LeadFromTheFrontEffect
  Cards.leadFromTheFront
  (modifiersL .~ [ThwartModifier 1, AttackModifier 1])

instance RunMessage LeadFromTheFrontEffect where
  runMessage msg e@(LeadFromTheFrontEffect attrs) = case msg of
    EndPhase _ -> e <$ push (EffectMessage (toId attrs) DisableEffect)
    _ -> LeadFromTheFrontEffect <$> runMessage msg attrs
