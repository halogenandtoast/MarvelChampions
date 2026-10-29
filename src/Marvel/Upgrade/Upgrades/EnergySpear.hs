module Marvel.Upgrade.Upgrades.EnergySpear (energySpear, EnergySpear (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

energySpear :: UpgradeCard EnergySpear
energySpear = upgrade EnergySpear Cards.energySpear

newtype EnergySpear = EnergySpear (Attrs Upgrade)
  deriving anyclass (HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade EnergySpear where
  upgradeTargets _ = Just . map toTarget <$>
    selectList (AllyWithTrait Guardian <> NotAlly (AllyWithUpgrade $ UpgradeIs Cards.energySpear))

instance HasModifiersFor EnergySpear where
  getModifiersFor _ (AllyRef aid) (EnergySpear attrs)
    | Just aid == upgradeAttachedAlly attrs = pure [AttackModifier 2, KeywordModifier Piercing]
  getModifiersFor _ _ _ = pure []

instance RunMessage EnergySpear where
  runMessage msg (EnergySpear attrs) = runQueueT $ EnergySpear <$> runMessage msg attrs
