module Marvel.Upgrade.Upgrades.LaserBlaster (
  laserBlaster,
  LaserBlaster (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

laserBlaster :: UpgradeCard LaserBlaster
laserBlaster = upgrade LaserBlaster Cards.laserBlaster

newtype LaserBlaster = LaserBlaster (Attrs Upgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance HasAbilities LaserBlaster where
  getAbilities _ = []

-- | "Attached ally gets +1 ATK and its attacks gain overkill."
instance HasModifiersFor LaserBlaster where
  getModifiersFor _ (AllyRef aid) (LaserBlaster attrs)
    | Just aid == upgradeAttachedAlly attrs = pure [AttackModifier 1, KeywordModifier Overkill]
  getModifiersFor _ _ _ = pure []

-- | "Attach to a Guardian ally. Max 1 per ally."
instance IsUpgrade LaserBlaster where
  upgradeTargets _ =
    Just . map toTarget <$> selectList (AllyWithTrait Guardian <> NotAlly (AllyWithUpgrade $ UpgradeIs Cards.laserBlaster))

instance RunMessage LaserBlaster where
  runMessage msg (LaserBlaster attrs) = LaserBlaster <$> runMessage msg attrs
