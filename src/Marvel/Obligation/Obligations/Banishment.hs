module Marvel.Obligation.Obligations.Banishment (
  banishment,
  Banishment (..),
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
import Marvel.Upgrade.Cards qualified as Upgrades

banishment :: ObligationCard Banishment
banishment = obligation Banishment Cards.banishment

newtype Banishment = Banishment (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | You may flip to alter-ego form (a hero not kept in its form). Choose: exhaust Peter
-- Quill → remove Banishment from the game (only while the identity is Peter Quill and ready: RR Target, Choose
-- (Option)); or discard an Element Gun from play (any player's, you pick), 3 threat on
-- the main scheme if there is none, and discard this obligation.
instance RunMessage Banishment where
  runMessage msg o@(Banishment attrs) = runQueueT $ case msg of
    ObligationMessage ident msg' | obligationId attrs == ident -> case msg' of
      RevealObligation identityId -> do
        canFlip <- identityMatches (HeroIdentity <> IdentityCanChangeForm) identityId
        when canFlip $
          chooseOne identityId
            [ labeled "$label.doNotFlipToAlterEgo" (pure ())
            , labeled "$label.changeToB" (changeToAlterEgo identityId)
            ]
        resolveObligation (obligationId attrs) identityId
        Banishment <$> runMessage msg attrs
      ResolveObligation identityId -> do
        readyQuill <- identityMatches (IdentityWithTitle "Peter Quill" <> UnexhaustedIdentity) identityId
        guns <- selectList (UpgradeIs Upgrades.elementGun)
        chooseOrRunOne identityId $
          [ labeled "$label.exhaustPeterQuillRemoveBanishment" (exhaust identityId >> removeFromGame attrs)
          | readyQuill
          ]
            <> [ labeled "$label.discardElementGunOrThreat" $
                  if null guns
                    then selectList MainScheme >>= traverse_ (`placeThreat` 3)
                    else chooseOne identityId [targeting gun (discardUpgrade gun) | gun <- guns]
               ]
        pure o
      _ -> Banishment <$> runMessage msg attrs
    _ -> Banishment <$> runMessage msg attrs
