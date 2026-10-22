module Marvel.Obligation.Obligations.LegalWork (
  legalWork,
  LegalWork (..),
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

legalWork :: ObligationCard LegalWork
legalWork = obligation LegalWork Cards.legalWork

newtype LegalWork = LegalWork (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage LegalWork where
  runMessage msg o@(LegalWork attrs) = runQueueT $ case msg of
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
          [ labeled "$label.exhaustJenniferWaltersRemoveLegalWork" (exhaust identityId >> removeFromGame attrs)
          | isUnexhaustedAlterEgo
          ]
            <> [labeled "$label.accelerationTokenDiscardObligation" addAccelerationToken]
        pure o
      _ -> LegalWork <$> runMessage msg attrs
    _ -> LegalWork <$> runMessage msg attrs
