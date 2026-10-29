module Marvel.Ally.Allies.VenomFlashThompson (venomFlashThompson, VenomFlashThompson (..)) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Count
import Marvel.Matchers

venomFlashThompson :: AllyCard VenomFlashThompson
venomFlashThompson = ally VenomFlashThompson Cards.venomFlashThompson (Thw 2, 2) (Atk 3, 2) (HP 4)

newtype VenomFlashThompson = VenomFlashThompson (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor VenomFlashThompson where
  getModifiersFor _ (AllyRef aid) (VenomFlashThompson attrs) | aid == allyId attrs = do
    threat <- selectCount SchemeThreat MainScheme
    pure [ConsequentialDamageReduction 1 | threat == 0]
  getModifiersFor _ _ _ = pure []

instance RunMessage VenomFlashThompson where
  runMessage msg (VenomFlashThompson attrs) = runQueueT $ VenomFlashThompson <$> runMessage msg attrs
