module Marvel.Upgrade.Upgrades.ReinforcedSuit (
  reinforcedSuit,
  ReinforcedSuit (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

reinforcedSuit :: UpgradeCard ReinforcedSuit
reinforcedSuit = upgrade ReinforcedSuit Cards.reinforcedSuit

newtype ReinforcedSuit = ReinforcedSuit (Attrs Upgrade)
  deriving anyclass (HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Attached ally gets +2 hit points.
instance HasModifiersFor ReinforcedSuit where
  getModifiersFor _ (AllyRef aid) (ReinforcedSuit a)
    | Just aid == upgradeAttachedAlly a = pure [HitPointModifier 2]
  getModifiersFor _ _ _ = pure []

-- | Attach to an ally. Max 1 per ally.
instance IsUpgrade ReinforcedSuit where
  upgradeTargets _ = Just . map toTarget <$> selectList (NotAlly $ AllyWithUpgrade $ UpgradeIs Cards.reinforcedSuit)

instance RunMessage ReinforcedSuit where
  runMessage msg (ReinforcedSuit attrs) = ReinforcedSuit <$> runMessage msg attrs
