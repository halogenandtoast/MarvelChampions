module Marvel.Upgrade.Upgrades.PantherClawsShuri where
import Marvel.Prelude
import Marvel.Id
import Marvel.Query
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage
import Marvel.Game.Source ()
import Marvel.BlackPantherShuri.Helpers ()
import Marvel.Keyword ()

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Upgrade.Types

pantherClawsShuri :: UpgradeCard PantherClawsShuri
pantherClawsShuri = upgrade PantherClawsShuri Cards.pantherClawsShuri
newtype PantherClawsShuri = PantherClawsShuri (Attrs Upgrade)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage PantherClawsShuri where
  runMessage msg u@(PantherClawsShuri attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> attackGuard you u do
      enemies <- selectList AttackableEnemy
      chooseOne you [targeting (enemyRef enemy) (cardSpecific attrs "enemy" (you,enemy)) | enemy <- enemies]
      pure u
    CardSpecific (isTarget attrs -> True) "enemy" (fromJSON -> Success (you :: IdentityId,enemy :: EnemyId)) -> do
      chooseOne you [labeled "$label.done" (dealDamage enemy attrs $ toDamage 2 $ FromPlayerAttack you), targeting attrs (discardUpgrade (upgradeId attrs) >> dealDamage enemy attrs (withPiercing $ toDamage 5 $ FromPlayerAttack you))]
      pure u
    _ -> PantherClawsShuri <$> runMessage msg attrs

instance HasAbilities PantherClawsShuri where
  getAbilities _ = []

instance IsUpgrade PantherClawsShuri where
  toUpgradeAttrs (PantherClawsShuri attrs) = attrs
  upgradeSpecialCanResolve (PantherClawsShuri _attrs) = selectAny AttackableEnemy
