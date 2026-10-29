module Marvel.Obligation.Obligations.EquipmentMalfunction (equipmentMalfunction, EquipmentMalfunction (..)) where

import Marvel.Prelude
import Marvel.Ability.Types (HasAbilities)
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Identity.Types (Field (PlayerIdentityCounters))
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted
import Marvel.Modifier (HasModifiersFor)
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Projection
import Marvel.Ref

equipmentMalfunction :: ObligationCard EquipmentMalfunction
equipmentMalfunction = obligation EquipmentMalfunction Cards.equipmentMalfunction

newtype EquipmentMalfunction = EquipmentMalfunction (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage EquipmentMalfunction where
  runMessage msg o@(EquipmentMalfunction attrs) = runQueueT $ case msg of
    ObligationMessage ident (RevealObligation you) | ident == obligationId attrs -> do
      hero <- identityMatches HeroIdentity you
      when hero $ chooseOne you
        [ labeled "$label.doNotFlipToAlterEgo" (pure ())
        , labeled "$label.changeToB" (changeToAlterEgo you)
        ]
      resolveObligation (obligationId attrs) you
      pure o
    ObligationMessage ident (ResolveObligation you) | ident == obligationId attrs -> do
      readyAlterEgo <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) you
      -- James Rhodes's forced form-change response resolves before this choice;
      -- count what this obligation actually removes, not what the hero had.
      ammo <- project PlayerIdentityCounters you
      chooseOrRunOne you $
        [ labeled "$label.exhaustJamesRhodesRemoveEquipmentMalfunction" (exhaust you >> removeFromGame attrs)
        | readyAlterEgo ]
        <> [labeled "$label.removeAllAmmoDiscardEquipmentMalfunction" do
              removeCounters you ammo
              when (ammo <= 2) (gainSurge attrs)]
      pure o
    _ -> EquipmentMalfunction <$> runMessage msg attrs
