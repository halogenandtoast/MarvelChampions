module Marvel.Obligation.Obligations.LegalWork
  ( legalWork
  , LegalWork(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Obligation.Attrs
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Target

legalWork :: ObligationCard LegalWork
legalWork = obligation LegalWork Cards.legalWork

newtype LegalWork = LegalWork ObligationAttrs
  deriving anyclass IsObligation
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage LegalWork where
  runMessage msg o@(LegalWork attrs) = case msg of
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
          isAlterEgo <- identityMatches AlterEgoIdentity identityId
          chooseOrRunOne identityId
            $ [ Label
                  "Exhaust Jennifer Walters-> remove Legal Work from the game."
                  [ Run
                      [ IdentityMessage identityId ExhaustedIdentity
                      , RemoveFromGame (toTarget attrs)
                      ]
                  ]
              | isAlterEgo
              ]
            <> [ Label
                   "Give the main scheme 1 acceleration token. Discard this obligation"
                   [Run [AddAccelerationToken]]
               ]
          pure o
        _ -> LegalWork <$> runMessage msg attrs
    _ -> LegalWork <$> runMessage msg attrs
