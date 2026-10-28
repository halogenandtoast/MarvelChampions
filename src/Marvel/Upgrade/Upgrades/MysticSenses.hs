module Marvel.Upgrade.Upgrades.MysticSenses (
  mysticSenses,
  MysticSenses (..),
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

mysticSenses :: UpgradeCard MysticSenses
mysticSenses = upgrade MysticSenses Cards.mysticSenses

newtype MysticSenses = MysticSenses (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Response: After you resolve Adam Warlock's "Battle Mage" ability, draw 1 card.
instance HasAbilities MysticSenses where
  getAbilities a@(MysticSenses attrs) =
    [ limitedWindowAbility
        a
        1
        (ResolvesAbility After (IdentityWithId $ upgradeController attrs) (CardIs Heroes.adamWarlockHero))
        HeroResponse
        OwnsThis
        NoCost
        (runAbility a 1)
    ]

instance RunMessage MysticSenses where
  runMessage msg u@(MysticSenses attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      drawCards you 1
      pure u
    _ -> MysticSenses <$> runMessage msg attrs
