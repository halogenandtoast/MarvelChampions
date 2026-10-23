module Marvel.Upgrade.Upgrades.PowerGloves (
  powerGloves,
  PowerGloves (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Message hiding (AllyAttacked, AllyThwarted)
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

powerGloves :: UpgradeCard PowerGloves
powerGloves = upgrade PowerGloves Cards.powerGloves

newtype PowerGloves = PowerGloves (Attrs Upgrade)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Response: After attached ally attacks or thwarts, deal 1 damage to an enemy.
instance HasAbilities PowerGloves where
  getAbilities (PowerGloves a) = case upgradeAttachedAlly a of
    Just aid ->
      [ limitedWindowAbility
          a
          1
          (WindowOneOf [AllyAttacked After (AllyWithId aid) AnyEnemy, AllyThwarted After (AllyWithId aid) AnyScheme])
          Response
          (OwnsThis <> EnemyExists AnyEnemy)
          NoCost
          $ ChooseDamage (toSource a) (toDamage 1 FromAbility) AnyEnemy
      ]
    Nothing -> []

-- | Attach to an Avenger ally. Max 1 per ally.
instance IsUpgrade PowerGloves where
  upgradeTargets _ =
    Just . map toTarget <$> selectList (AllyWithTrait Avenger <> NotAlly (AllyWithUpgrade $ UpgradeIs Cards.powerGloves))

instance RunMessage PowerGloves where
  runMessage msg (PowerGloves attrs) = PowerGloves <$> runMessage msg attrs
