module Marvel.Upgrade.Upgrades.TargetingScreen (
  targetingScreen,
  TargetingScreen (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

targetingScreen :: UpgradeCard TargetingScreen
targetingScreen = upgrade TargetingScreen Cards.targetingScreen

newtype TargetingScreen = TargetingScreen (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: Exhaust Targeting Screen → remove 2 threat from a scheme (3 threat
-- instead if you control the Milano). Not a thwart: patrol does not keep it off the
-- main scheme.
instance HasAbilities TargetingScreen where
  getAbilities a =
    [ability a 1 HeroAction (OwnsThis <> SchemeExists ThwartableScheme) ExhaustCost (runAbility a 1)]

instance RunMessage TargetingScreen where
  runMessage msg u@(TargetingScreen attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      milano <- controlsMilano you
      removeThreat you attrs (if milano then 3 else 2) ThwartableScheme
      pure u
    _ -> TargetingScreen <$> runMessage msg attrs
