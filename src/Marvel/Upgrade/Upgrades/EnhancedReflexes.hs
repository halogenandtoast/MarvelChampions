module Marvel.Upgrade.Upgrades.EnhancedReflexes (
  enhancedReflexes,
  EnhancedReflexes (..),
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

enhancedReflexes :: UpgradeCard EnhancedReflexes
enhancedReflexes =
  upgradeWith EnhancedReflexes Cards.enhancedReflexes $
    (usesL .~ 3) . (discardIfNoUsesL .~ True)

newtype EnhancedReflexes = EnhancedReflexes (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities EnhancedReflexes where
  getAbilities (EnhancedReflexes a) =
    [ ability a 1 HeroResource OwnsThis (ExhaustCost <> UseCost) (Pay $ ResourcePayment Energy)
    ]

instance RunMessage EnhancedReflexes where
  runMessage msg (EnhancedReflexes attrs) = EnhancedReflexes <$> runMessage msg attrs
