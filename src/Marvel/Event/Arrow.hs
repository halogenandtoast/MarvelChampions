-- | Hawkeye's Arrow attacks: "Hawkeye's Bow: each of your Arrow attacks gain ranged."
module Marvel.Event.Arrow (arrowDamage) where

import Marvel.Prelude

import Marvel.Damage
import Marvel.Id
import Marvel.Matchers
import Marvel.Query
import Marvel.Upgrade.Cards qualified as Upgrades

-- | The player's Arrow attack damage, ranged while they control Hawkeye's Bow.
arrowDamage :: (Query m UpgradeMatcher) => IdentityId -> Natural -> m Damage
arrowDamage ident n = do
  bow <- selectAny $ UpgradeIs Upgrades.hawkeyesBow <> upgradeControlledBy ident
  pure $ (if bow then withRanged else id) $ toDamage n (FromPlayerAttack ident)
