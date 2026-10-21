module Marvel.Obligation.Obligations.FamilyEmergency (
  familyEmergency,
  FamilyEmergency (..),
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

familyEmergency :: ObligationCard FamilyEmergency
familyEmergency = obligation FamilyEmergency Cards.familyEmergency

newtype FamilyEmergency = FamilyEmergency (Attrs Obligation)
  deriving anyclass (IsObligation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage FamilyEmergency where
  runMessage msg o@(FamilyEmergency attrs) = case msg of
    ObligationMessage ident msg' | obligationId attrs == ident -> case msg' of
      RevealObligation identityId -> do
        isHero <- identityMatches HeroIdentity identityId
        pushAll $
          [ Ask identityId $
            ChooseOne
              [ Label "$label.doNotFlipToAlterEgo" []
              , Label "$label.changeToB" [ChangeForm]
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
            "$label.exhaustCarolDanversRemoveFamilyEmergency"
            [ Run
                [ IdentityMessage identityId ExhaustedIdentity
                , RemoveFromGame (toRef attrs)
                ]
            ]
          | isUnexhaustedAlterEgo
          ]
            <> [ Label
                  "$label.stunnedSurgeDiscardObligation"
                  [Run [IdentityMessage identityId IdentityStunned, Surge identityId]]
               ]
        pure o
      _ -> FamilyEmergency <$> runMessage msg attrs
    _ -> FamilyEmergency <$> runMessage msg attrs
