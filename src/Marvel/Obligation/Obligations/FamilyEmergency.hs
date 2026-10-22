module Marvel.Obligation.Obligations.FamilyEmergency (
  familyEmergency,
  FamilyEmergency (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Ability.Types (HasAbilities)
import Marvel.Modifier (HasModifiersFor)

familyEmergency :: ObligationCard FamilyEmergency
familyEmergency = obligation FamilyEmergency Cards.familyEmergency

newtype FamilyEmergency = FamilyEmergency (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage FamilyEmergency where
  runMessage msg o@(FamilyEmergency attrs) = runQueueT $ case msg of
    ObligationMessage ident msg' | obligationId attrs == ident -> case msg' of
      RevealObligation identityId -> do
        isHero <- identityMatches HeroIdentity identityId
        when isHero $
          chooseOne identityId
            [ labeled "$label.doNotFlipToAlterEgo" (pure ())
            , labeled "$label.changeToB" (changeForm identityId)
            ]
        resolveObligation (obligationId attrs) identityId
        pure o
      ResolveObligation identityId -> do
        isUnexhaustedAlterEgo <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) identityId
        chooseOrRunOne identityId $
          [ labeled "$label.exhaustCarolDanversRemoveFamilyEmergency" (exhaust identityId >> removeFromGame attrs)
          | isUnexhaustedAlterEgo
          ]
            <> [labeled "$label.stunnedSurgeDiscardObligation" (stunIdentity identityId >> surge identityId)]
        pure o
      _ -> FamilyEmergency <$> runMessage msg attrs
    _ -> FamilyEmergency <$> runMessage msg attrs
