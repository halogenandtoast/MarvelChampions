module Marvel.Upgrade.Upgrades.HeavyCannon (
  heavyCannon,
  HeavyCannon (..),
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

heavyCannon :: UpgradeCard HeavyCannon
heavyCannon = upgrade HeavyCannon Cards.heavyCannon

newtype HeavyCannon = HeavyCannon (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: Exhaust Heavy Cannon → deal 1 damage to each enemy. If you control the
-- Milano, deal 1 additional damage to the villain. Not an attack. "Additional" alters
-- the amount (RR Alteration Effect): the villain takes 2 as one instance, with the rest
-- as one effect.
instance HasAbilities HeavyCannon where
  getAbilities a =
    [ability a 1 HeroAction (OwnsThis <> EnemyExists DamageableEnemy) ExhaustCost (runAbility a 1)]

instance RunMessage HeavyCannon where
  runMessage msg u@(HeavyCannon attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      milano <- controlsMilano you
      enemies <- selectList DamageableEnemy
      -- "the villain": the active one, when there are several
      active <- selectOne ActiveVillain
      let amount = \case
            EnemyVillainId v | milano, Just v == active -> 2
            _ -> 1
      dealDamagesToEach attrs [(enemy, toDamage (amount enemy) FromAbility) | enemy <- enemies]
      pure u
    _ -> HeavyCannon <$> runMessage msg attrs
