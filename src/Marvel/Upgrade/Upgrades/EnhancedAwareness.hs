module Marvel.Upgrade.Upgrades.EnhancedAwareness (
  enhancedAwareness,
  EnhancedAwareness (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Payment
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

enhancedAwareness :: UpgradeCard EnhancedAwareness
enhancedAwareness =
  upgradeWith EnhancedAwareness Cards.enhancedAwareness $
    (usesL .~ 3)
      . (discardIfNoUsesL .~ True)

newtype EnhancedAwareness = EnhancedAwareness (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities EnhancedAwareness where
  getAbilities (EnhancedAwareness a) =
    [ability a 1 HeroResource OwnsThis (ExhaustCost <> UseCost) (Pay $ ResourcePayment Mental)]

instance RunMessage EnhancedAwareness where
  runMessage msg (EnhancedAwareness attrs) = EnhancedAwareness <$> runMessage msg attrs
