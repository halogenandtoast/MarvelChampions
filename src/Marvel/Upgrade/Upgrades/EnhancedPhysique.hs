module Marvel.Upgrade.Upgrades.EnhancedPhysique (
  enhancedPhysique,
  EnhancedPhysique (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

enhancedPhysique :: UpgradeCard EnhancedPhysique
enhancedPhysique =
  upgradeWith EnhancedPhysique Cards.enhancedPhysique $
    (usesL .~ 3)
      . (discardIfNoUsesL .~ True)

newtype EnhancedPhysique = EnhancedPhysique (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities EnhancedPhysique where
  getAbilities (EnhancedPhysique a) =
    [ability a 1 HeroResource OwnsThis (ExhaustCost <> UseCost) (Pay $ ResourcePayment Physical)]

instance RunMessage EnhancedPhysique where
  runMessage msg (EnhancedPhysique attrs) = EnhancedPhysique <$> runMessage msg attrs
