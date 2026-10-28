module Marvel.Upgrade.Upgrades.WarlocksCape (
  warlocksCape,
  WarlocksCape (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Hero.Cards qualified as Heroes
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

warlocksCape :: UpgradeCard WarlocksCape
warlocksCape = upgrade WarlocksCape Cards.warlocksCape

newtype WarlocksCape = WarlocksCape (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Response: After you resolve Adam Warlock's "Battle Mage" ability (the only
-- ability printed on his hero face), exhaust Warlock's Cape → ready Adam Warlock.
instance HasAbilities WarlocksCape where
  getAbilities a@(WarlocksCape attrs) =
    [ limitedWindowAbility
        a
        1
        (ResolvesAbility After (IdentityWithId $ upgradeController attrs) (CardIs Heroes.adamWarlockHero))
        HeroResponse
        OwnsThis
        ExhaustCost
        (runAbility a 1)
    ]

instance RunMessage WarlocksCape where
  runMessage msg u@(WarlocksCape attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      ready you
      pure u
    _ -> WarlocksCape <$> runMessage msg attrs
