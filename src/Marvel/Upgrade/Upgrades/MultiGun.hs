module Marvel.Upgrade.Upgrades.MultiGun (
  multiGun,
  MultiGun (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

multiGun :: UpgradeCard MultiGun
multiGun = upgrade MultiGun Cards.multiGun

newtype MultiGun = MultiGun (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: exhaust Multi-Gun → choose one: deal 2 damage to an enemy; choose a
-- player, deal 1 damage to each minion engaged with that player; remove 2 threat from a
-- scheme.
instance HasAbilities MultiGun where
  getAbilities (MultiGun a) = [ability a 1 HeroAction OwnsThis ExhaustCost $ runAbility a 1]

-- | An option that cannot be resolved is not offered (RR Choose (Option)): the minion
-- option, and its players, need a minion engaged; removing threat, a scheme it can be
-- removed from.
instance RunMessage MultiGun where
  runMessage msg u@(MultiGun attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      players <- getPlayers
      engaged <-
        filter (not . null . snd)
          <$> traverse (\p -> (p,) <$> selectList (MinionEngagedWith $ IdentityWithId p)) players
      threat <- selectAny ThwartableScheme
      chooseOne ident $
        [labeled (countLabel "dealDamageToAnEnemy" 2) (chooseDamage ident attrs (toDamage 2 FromAbility) AnyEnemy)]
          <> [ labeled (countLabel "dealDamageToEachMinionEngagedWithAPlayer" 1) $
                chooseOne ident
                  [targeting p (dealDamageToEach (map toEnemy minions) attrs (toDamage 1 FromAbility)) | (p, minions) <- engaged]
             | notNull engaged
             ]
          <> [labeled (countLabel "removeThreatFromAScheme" 2) (removeThreat ident attrs 2 ThwartableScheme) | threat]
      pure u
    _ -> MultiGun <$> runMessage msg attrs
