module Marvel.Obligation.Obligations.BurnNotice (
  burnNotice,
  BurnNotice (..),
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
import Marvel.Query
import Marvel.Trait

burnNotice :: ObligationCard BurnNotice
burnNotice = obligation BurnNotice Cards.burnNotice

newtype BurnNotice = BurnNotice (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage BurnNotice where
  runMessage msg o@(BurnNotice attrs) = runQueueT $ case msg of
    ObligationMessage ident msg' | obligationId attrs == ident -> case msg' of
      RevealObligation identityId -> do
        isHero <- identityMatches HeroIdentity identityId
        when isHero $
          chooseOne identityId
            [ labeled "$label.doNotFlipToAlterEgo" (pure ())
            , labeled "$label.changeToB" (changeToAlterEgo identityId)
            ]
        resolveObligation (obligationId attrs) identityId
        pure o
      ResolveObligation identityId -> do
        isUnexhaustedAlterEgo <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) identityId
        -- the highest cost may be shared: the player chooses among those
        highest <-
          selectRefs $
            UpgradeWithHighestCostAmong
              (UpgradeWithTrait Preparation <> UpgradeControlledBy (IdentityWithId identityId))
        chooseOrRunOne identityId $
          [ labeled "$label.exhaustNatashaRomanoffRemoveBurnNotice" (exhaust identityId >> removeFromGame attrs)
          | isUnexhaustedAlterEgo
          ]
            <> [ labeled "$label.discardHighestCostPreparationDiscardObligation" $
                  if null highest
                    then gainSurge attrs
                    else chooseOne identityId [targeting target (removeFromPlay target) | target <- highest]
               ]
        pure o
      _ -> BurnNotice <$> runMessage msg attrs
    _ -> BurnNotice <$> runMessage msg attrs
