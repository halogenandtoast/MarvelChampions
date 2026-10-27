module Marvel.Upgrade.Upgrades.SonicRifle (
  sonicRifle,
  SonicRifle (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

sonicRifle :: UpgradeCard SonicRifle
sonicRifle =
  upgradeWith SonicRifle Cards.sonicRifle $
    (usesL .~ 2)
      . (discardIfNoUsesL .~ True)

newtype SonicRifle = SonicRifle (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: Exhaust Sonic Rifle and remove 1 charge counter from it → confuse an
-- enemy (deal 3 damage to that enemy instead if it is already confused).
instance HasAbilities SonicRifle where
  getAbilities a = [ability a 1 HeroAction OwnsThis (ExhaustCost <> UseCost) $ runAbility a 1]

instance RunMessage SonicRifle where
  runMessage msg u@(SonicRifle attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      enemies <- selectList AnyEnemy
      chooseOne ident
        [ targeting (enemyRef enemy) do
            confused <- selectAny $ EnemyWithId enemy <> ConfusedEnemy
            if confused
              then dealDamage enemy attrs (toDamage 3 FromAbility)
              else confuse enemy attrs
        | enemy <- enemies
        ]
      pure u
    _ -> SonicRifle <$> runMessage msg attrs
