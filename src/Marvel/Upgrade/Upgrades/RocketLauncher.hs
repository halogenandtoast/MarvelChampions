module Marvel.Upgrade.Upgrades.RocketLauncher (
  rocketLauncher,
  RocketLauncher (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
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

-- | 2 charge counters, and no uses keyword: it stays in play with none (FAQ).
rocketLauncher :: UpgradeCard RocketLauncher
rocketLauncher = upgradeWith RocketLauncher Cards.rocketLauncher (usesL .~ 2)

newtype RocketLauncher = RocketLauncher (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: Exhaust Rocket Launcher and remove 1 charge counter from it → choose a
-- player. Deal 2 damage to the villain and each minion engaged with that player. Not an
-- attack: damage from an ability, as one effect.
instance HasAbilities RocketLauncher where
  getAbilities a =
    [ability a 1 HeroAction OwnsThis (ExhaustCost <> UseCost) $ runAbility a 1]

instance RunMessage RocketLauncher where
  runMessage msg u@(RocketLauncher attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      players <- getPlayers
      withTheVillainIfAny ident \villain -> chooseOne ident
        [ targeting p do
            minions <- selectList $ MinionEngagedWith (IdentityWithId p)
            dealDamageToEach (maybe [] (pure . EnemyVillainId) villain <> map EnemyMinionId minions) attrs (toDamage 2 FromAbility)
        | p <- players
        ]
      pure u
    _ -> RocketLauncher <$> runMessage msg attrs
