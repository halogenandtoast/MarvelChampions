module Marvel.Obligation.Obligations.BusinessProblems (
  businessProblems,
  BusinessProblems (..),
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

businessProblems :: ObligationCard BusinessProblems
businessProblems = obligation BusinessProblems Cards.businessProblems

newtype BusinessProblems = BusinessProblems (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage BusinessProblems where
  runMessage msg o@(BusinessProblems attrs) = runQueueT $ case msg of
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
        ironManUpgrades <- selectList $ UpgradeControlledBy (IdentityWithId identityId)
        chooseOrRunOne identityId $
          [ labeled "$label.exhaustTonyStarkRemoveBusinessProblems" (exhaust identityId >> removeFromGame attrs)
          | isUnexhaustedAlterEgo
          ]
            <> [labeled "$label.exhaustEachUpgradeDiscardObligation" (for_ ironManUpgrades exhaust)]
        pure o
      _ -> BusinessProblems <$> runMessage msg attrs
    _ -> BusinessProblems <$> runMessage msg attrs
