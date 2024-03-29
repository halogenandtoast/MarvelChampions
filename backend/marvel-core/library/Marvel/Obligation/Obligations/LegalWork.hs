module Marvel.Obligation.Obligations.LegalWork (
  legalWork,
  LegalWork (..),
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

legalWork :: ObligationCard LegalWork
legalWork = obligation LegalWork Cards.legalWork

newtype LegalWork = LegalWork (Attrs Obligation)
  deriving anyclass (IsObligation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage LegalWork where
  runMessage msg o@(LegalWork attrs) = case msg of
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
            "Exhaust Jennifer Walters-> remove Legal Work from the game."
            [ Run
                [ IdentityMessage identityId ExhaustedIdentity
                , RemoveFromGame (toTarget attrs)
                ]
            ]
          | isUnexhaustedAlterEgo
          ]
            <> [ Label
                  "Give the main scheme 1 acceleration token. Discard this obligation"
                  [Run [AddAccelerationToken]]
               ]
        pure o
      _ -> LegalWork <$> runMessage msg attrs
    _ -> LegalWork <$> runMessage msg attrs
