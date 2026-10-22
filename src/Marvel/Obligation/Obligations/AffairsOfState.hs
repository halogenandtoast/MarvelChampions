module Marvel.Obligation.Obligations.AffairsOfState (
  affairsOfState,
  AffairsOfState (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Query
import Marvel.Trait

affairsOfState :: ObligationCard AffairsOfState
affairsOfState = obligation AffairsOfState Cards.affairsOfState

newtype AffairsOfState = AffairsOfState (Attrs Obligation)
  deriving anyclass (IsObligation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage AffairsOfState where
  runMessage msg o@(AffairsOfState attrs) = runQueueT $ case msg of
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
        blackPantherUpgrades <-
          selectRefs $
            UpgradeWithTrait BlackPanther
              <> UpgradeControlledBy (IdentityWithId identityId)
        chooseOrRunOne identityId $
          [ labeled "$label.exhaustTChallaRemoveAffairsOfState" (exhaust identityId >> removeFromGame attrs)
          | isUnexhaustedAlterEgo
          ]
            <> [ labeled "$label.discardBlackPantherUpgradeDiscardObligation" $
                  chooseOne identityId [targeting target (removeFromPlay target) | target <- blackPantherUpgrades]
               ]
        pure o
      _ -> AffairsOfState <$> runMessage msg attrs
    _ -> AffairsOfState <$> runMessage msg attrs
