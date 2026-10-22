module Marvel.Upgrade.Upgrades.VibraniumSuit (
  vibraniumSuit,
  VibraniumSuit (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Count
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Minion.Cards qualified as Cards
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

vibraniumSuit :: UpgradeCard VibraniumSuit
vibraniumSuit = upgrade VibraniumSuit Cards.vibraniumSuit

newtype VibraniumSuit = VibraniumSuit (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance HasAbilities VibraniumSuit where
  getAbilities _ = []

instance RunMessage VibraniumSuit where
  runMessage msg u@(VibraniumSuit attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> attackGuard ident u do
      modifiers <- getModifiers attrs
      sustainedDamage <-
        selectCount
          SustainedDamage
          (IdentityWithId $ upgradeController attrs)
      let
        dmg =
          min sustainedDamage $
            if LastSpecial `elem` modifiers then 2 else 1
      chooseDamage (upgradeController attrs) attrs (toDamage dmg $ FromPlayerAttack ident) $
        AttackableBy (IdentityWithId ident) <> NotEnemy (EnemyIs Cards.killmonger)
      heal (upgradeController attrs) dmg
      pure u
    _ -> VibraniumSuit <$> runMessage msg attrs
