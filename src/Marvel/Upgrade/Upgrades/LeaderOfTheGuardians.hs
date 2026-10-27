module Marvel.Upgrade.Upgrades.LeaderOfTheGuardians (
  leaderOfTheGuardians,
  LeaderOfTheGuardians (..),
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

leaderOfTheGuardians :: UpgradeCard LeaderOfTheGuardians
leaderOfTheGuardians = upgrade LeaderOfTheGuardians Cards.leaderOfTheGuardians

newtype LeaderOfTheGuardians = LeaderOfTheGuardians (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Each Guardian character you control gets +1 THW: your identity if it is a Guardian,
-- and your Guardian allies, Star-Lord's among them. ponytail: the trait is read through
-- 'getModifiers', whose depth guard stops this asking itself forever.
instance HasModifiersFor LeaderOfTheGuardians where
  getModifiersFor _ (IdentityRef ident) (LeaderOfTheGuardians a)
    | ident == upgradeController a = do
        guardian <- selectAny (IdentityWithId ident <> IdentityWithTrait Guardian)
        pure [ThwartModifier 1 | guardian]
  getModifiersFor _ (AllyRef aid) (LeaderOfTheGuardians a) = do
    guardian <- selectAny (AllyWithId aid <> AllyControlledBy (IdentityWithId $ upgradeController a) <> AllyWithTrait Guardian)
    pure [ThwartModifier 1 | guardian]
  getModifiersFor _ _ _ = pure []

instance RunMessage LeaderOfTheGuardians where
  runMessage msg (LeaderOfTheGuardians attrs) = LeaderOfTheGuardians <$> runMessage msg attrs
