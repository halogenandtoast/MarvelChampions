module Marvel.Upgrade.Upgrades.HyperThrusters (
  hyperThrusters,
  HyperThrusters (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

hyperThrusters :: UpgradeCard HyperThrusters
hyperThrusters = upgrade HyperThrusters Cards.hyperThrusters

newtype HyperThrusters = HyperThrusters (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: Exhaust Hyper Thrusters → remove 1 threat from each scheme. If you
-- control the Milano, remove 1 additional threat from the main scheme. Not a thwart:
-- patrol and "cannot be removed by thwarting" do not stop it; a crisis icon or "threat
-- cannot be removed" does (RR Target: each scheme threat can be removed from).
instance HasAbilities HyperThrusters where
  getAbilities a =
    [ability a 1 HeroAction (OwnsThis <> SchemeExists ThwartableScheme) ExhaustCost (runAbility a 1)]

instance RunMessage HyperThrusters where
  runMessage msg u@(HyperThrusters attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      milano <- controlsMilano you
      schemes <- selectList ThwartableScheme
      thwartEach attrs [(scheme, amount) | scheme <- schemes, let amount = if milano && isMain scheme then 2 else 1]
      pure u
    _ -> HyperThrusters <$> runMessage msg attrs
   where
    isMain = \case
      SchemeMainSchemeId _ -> True
      _ -> False
