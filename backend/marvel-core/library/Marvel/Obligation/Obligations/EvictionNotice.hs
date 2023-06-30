module Marvel.Obligation.Obligations.EvictionNotice (
  evictionNotice,
  EvictionNotice (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Entity
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Question
import Marvel.Queue
import Marvel.Ref

evictionNotice :: ObligationCard EvictionNotice
evictionNotice = obligation EvictionNotice Cards.evictionNotice

newtype EvictionNotice = EvictionNotice (Attrs Obligation)
  deriving anyclass (IsObligation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage EvictionNotice where
  runMessage msg o@(EvictionNotice attrs) = case msg of
    ObligationMessage ident msg' | obligationId attrs == ident -> case msg' of
      RevealObligation identityId -> do
        isHero <- identityMatches HeroIdentity identityId
        pushAll $
          [ Ask identityId $
            ChooseOne
              [ Label "Do not flip to alter-ego" []
              , Label "Flip to alter-ego" [ChangeForm]
              ]
          | isHero
          ]
            <> [ ObligationMessage (obligationId attrs) $
                  ResolveObligation identityId
               ]
        pure o
      ResolveObligation identityId -> do
        isUnexhaustedAlterEgo <-
          identityMatches
            (AlterEgoIdentity <> UnexhaustedIdentity)
            identityId
        chooseOrRunOne identityId $
          [ Label
            "Exhaust Peter Parker -> remove EvictionNotice from the game."
            [ Run
                [ IdentityMessage identityId ExhaustedIdentity
                , RemoveFromGame (toTarget attrs)
                ]
            ]
          | isUnexhaustedAlterEgo
          ]
            <> [ Label
                  "Discard 1 card at random from your hand. This card gains surge, Discard this obligation"
                  [ Run
                      [ IdentityMessage identityId $
                          DiscardFrom RandomFromHand 1 Nothing
                      , GainSurge (toTarget attrs)
                      ]
                  ]
               ]
        pure o
      _ -> EvictionNotice <$> runMessage msg attrs
    _ -> EvictionNotice <$> runMessage msg attrs
