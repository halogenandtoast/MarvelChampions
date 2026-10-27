module Marvel.Upgrade.Upgrades.CommsImplant (
  commsImplant,
  CommsImplant (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

commsImplant :: UpgradeCard CommsImplant
commsImplant = upgrade CommsImplant Cards.commsImplant

newtype CommsImplant = CommsImplant (Attrs Upgrade)
  deriving anyclass (HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Attached ally gets +1 THW and +1 hit point."
instance HasModifiersFor CommsImplant where
  getModifiersFor _ (AllyRef aid) (CommsImplant attrs)
    | Just aid == upgradeAttachedAlly attrs = pure [ThwartModifier 1, HitPointModifier 1]
  getModifiersFor _ _ _ = pure []

-- | "Attach to a Guardian ally. Max 1 per ally."
instance IsUpgrade CommsImplant where
  upgradeTargets _ =
    Just . map toTarget <$> selectList (AllyWithTrait Guardian <> NotAlly (AllyWithUpgrade $ UpgradeIs Cards.commsImplant))

instance RunMessage CommsImplant where
  runMessage msg (CommsImplant attrs) = CommsImplant <$> runMessage msg attrs
