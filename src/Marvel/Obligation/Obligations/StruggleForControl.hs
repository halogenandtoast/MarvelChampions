module Marvel.Obligation.Obligations.StruggleForControl (
  struggleForControl,
  StruggleForControl (..),
) where

import Marvel.Prelude

import Marvel.Ability.Types (HasAbilities)
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Identity.Types (Field (..))
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier (HasModifiersFor)
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Projection
import Marvel.Query

struggleForControl :: ObligationCard StruggleForControl
struggleForControl = obligation StruggleForControl Cards.struggleForControl

newtype StruggleForControl = StruggleForControl (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | You may flip to alter-ego form. Choose: exhaust Flash Thompson and take 2 damage
-- (a cost: only with a ready alter-ego and no tough status, RR Choose (Option)); or put
-- 1 Enraged Symbiote still set aside into play engaged with the first player, gaining
-- surge with none left. Either way it is discarded.
instance RunMessage StruggleForControl where
  runMessage msg o@(StruggleForControl attrs) = runQueueT $ case msg of
    ObligationMessage ident msg' | obligationId attrs == ident -> case msg' of
      RevealObligation identityId -> do
        canFlip <- identityMatches (HeroIdentity <> IdentityCanChangeForm) identityId
        when canFlip $
          chooseOne identityId
            [ labeled "$label.doNotFlipToAlterEgo" (pure ())
            , labeled "$label.changeToB" (changeToAlterEgo identityId)
            ]
        resolveObligation (obligationId attrs) identityId
        StruggleForControl <$> runMessage msg attrs
      ResolveObligation identityId -> do
        readyAlterEgo <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) identityId
        notTough <- projectP PlayerIdentityTough not identityId
        symbiotes <- filter (cardMatch (CardIs Minions.enragedSymbiote)) <$> getSetAsideEncounterCards
        first <- selectJust FirstPlayer
        chooseOrRunOne identityId $
          [ labeled "$label.exhaustFlashThompsonTakeDamage" do
              exhaust identityId
              damageIdentity identityId attrs (toDamage 2 FromAbility)
          | readyAlterEgo && notTough
          ]
            <> [ labeled "$label.putEnragedSymbioteIntoPlayOrSurge" case symbiotes of
                  (symbiote : _) -> putEncounterCardIntoPlay first symbiote
                  [] -> gainSurge attrs
               ]
        pure o
      _ -> StruggleForControl <$> runMessage msg attrs
    _ -> StruggleForControl <$> runMessage msg attrs
