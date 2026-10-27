module Marvel.Obligation.Obligations.CrisisOnHalfworld (
  crisisOnHalfworld,
  CrisisOnHalfworld (..),
) where

import Marvel.Prelude

import Marvel.Ability.Types (HasAbilities)
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted
import Marvel.Modifier (HasModifiersFor)
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Query

crisisOnHalfworld :: ObligationCard CrisisOnHalfworld
crisisOnHalfworld = obligation CrisisOnHalfworld Cards.crisisOnHalfworld

newtype CrisisOnHalfworld = CrisisOnHalfworld (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | You may flip to alter-ego form. Choose: exhaust your alter-ego → remove this from the
-- game (only while a ready alter-ego: RR Choose (Option)); or discard the highest cost
-- upgrade you control (ties: you choose), and if none was, this card gains surge; then
-- discard this obligation.
instance RunMessage CrisisOnHalfworld where
  runMessage msg o@(CrisisOnHalfworld attrs) = runQueueT $ case msg of
    ObligationMessage ident msg' | obligationId attrs == ident -> case msg' of
      RevealObligation identityId -> do
        canFlip <- identityMatches (HeroIdentity <> IdentityCanChangeForm) identityId
        when canFlip $
          chooseOne identityId
            [ labeled "$label.doNotFlipToAlterEgo" (pure ())
            , labeled "$label.changeToB" (changeToAlterEgo identityId)
            ]
        resolveObligation (obligationId attrs) identityId
        CrisisOnHalfworld <$> runMessage msg attrs
      ResolveObligation identityId -> do
        readyAlterEgo <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) identityId
        highest <- selectList $ UpgradeWithHighestCostAmong (upgradeControlledBy identityId)
        chooseOrRunOne identityId $
          [ labeled "$label.exhaustRocketRaccoonRemoveCrisisOnHalfworld" (exhaust identityId >> removeFromGame attrs)
          | readyAlterEgo
          ]
            <> [ labeled "$label.discardHighestCostUpgradeDiscardObligation" $
                  if null highest
                    then gainSurge attrs
                    else chooseOrRunOne identityId [targeting u (discardUpgrade u) | u <- highest]
               ]
        pure o
      _ -> CrisisOnHalfworld <$> runMessage msg attrs
    _ -> CrisisOnHalfworld <$> runMessage msg attrs
