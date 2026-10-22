module Marvel.Upgrade.Upgrades.PantherClaws (
  pantherClaws,
  PantherClaws (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Cards
import Marvel.Modifier
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

pantherClaws :: UpgradeCard PantherClaws
pantherClaws = upgrade PantherClaws Cards.pantherClaws

newtype PantherClaws = PantherClaws (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance HasAbilities PantherClaws where
  getAbilities _ = []

instance RunMessage PantherClaws where
  runMessage msg u@(PantherClaws attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> attackGuard ident u do
      modifiers <- getModifiers attrs
      let dmg = if LastSpecial `elem` modifiers then 4 else 2
      chooseDamage ident attrs (toDamage dmg $ FromPlayerAttack ident) (AttackableBy (IdentityWithId ident) <> NotEnemy (EnemyIs Cards.killmonger))
      pure u
    _ -> PantherClaws <$> runMessage msg attrs
