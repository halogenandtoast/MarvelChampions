module Marvel.Obligation.Obligations.EvictionNotice
  ( evictionNotice
  , EvictionNotice(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message
import Marvel.Obligation.Attrs
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Target

evictionNotice :: ObligationCard EvictionNotice
evictionNotice = obligation EvictionNotice Cards.evictionNotice

newtype EvictionNotice = EvictionNotice ObligationAttrs
  deriving anyclass IsObligation
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage EvictionNotice where
  runMessage msg o@(EvictionNotice attrs) = case msg of
    ObligationMessage obligationId msg' | toId attrs == obligationId ->
      case msg' of
        RevealObligation identityId -> do
          isHero <- identityMatches HeroIdentity identityId
          pushAll
            $ [ Ask identityId $ ChooseOne
                  [ Label "Do not flip to alter-ego" []
                  , Label "Flip to alter-ego" [ChangeForm]
                  ]
              | isHero
              ]
            <> [ObligationMessage (toId attrs) $ ResolveObligation identityId]
          pure o
        ResolveObligation identityId -> do
          isUnexhaustedAlterEgo <- identityMatches
            (AlterEgoIdentity <> UnexhaustedIdentity)
            identityId
          chooseOrRunOne identityId
            $ [ Label
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
                       [ IdentityMessage identityId
                         $ DiscardFrom RandomFromHand 1 Nothing
                       , GainSurge (toTarget attrs)
                       ]
                   ]
               ]
          pure o
        _ -> EvictionNotice <$> runMessage msg attrs
    _ -> EvictionNotice <$> runMessage msg attrs
