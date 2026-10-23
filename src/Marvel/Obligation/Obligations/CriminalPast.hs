module Marvel.Obligation.Obligations.CriminalPast (
  criminalPast,
  CriminalPast (..),
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
import Marvel.Upgrade.Cards qualified as Upgrades

criminalPast :: ObligationCard CriminalPast
criminalPast = obligation CriminalPast Cards.criminalPast

newtype CriminalPast = CriminalPast (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | You may flip to alter-ego form. Choose: exhaust Clint Barton → remove this card
-- from the game; or discard Hawkeye's Bow from play and discard this obligation.
instance RunMessage CriminalPast where
  runMessage msg o@(CriminalPast attrs) = runQueueT $ case msg of
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
        canExhaust <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) identityId
        bows <- selectList $ UpgradeIs Upgrades.hawkeyesBow <> upgradeControlledBy identityId
        chooseOrRunOne identityId $
          [ labeled "$label.exhaustClintBartonRemoveCriminalPast" (exhaust identityId >> removeFromGame attrs)
          | canExhaust
          ]
            <> [labeled "$label.discardHawkeyesBowDiscardObligation" (for_ bows removeFromPlay)]
        pure o
      _ -> CriminalPast <$> runMessage msg attrs
    _ -> CriminalPast <$> runMessage msg attrs
