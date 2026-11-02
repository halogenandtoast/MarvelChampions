module Marvel.Upgrade.Upgrades.KimoyoBeadsShuri where
import Marvel.Prelude
import Marvel.Id
import Marvel.Query
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.BlackPantherShuri.Helpers ()

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Upgrade.Types

kimoyoBeadsShuri :: UpgradeCard KimoyoBeadsShuri
kimoyoBeadsShuri = upgrade KimoyoBeadsShuri Cards.kimoyoBeadsShuri
newtype KimoyoBeadsShuri = KimoyoBeadsShuri (Attrs Upgrade)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage KimoyoBeadsShuri where
  runMessage msg u@(KimoyoBeadsShuri attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> thwartGuard u do
      removeThreat you attrs 1 ThwartableScheme
      cardSpecific attrs "confuse" you
      pure u
    CardSpecific (isTarget attrs -> True) "confuse" (fromJSON -> Success (you :: IdentityId)) -> do
      enemies <- selectList EnemyCanBeConfused
      chooseOne you (labeled "$label.done" (pure ()) : [targeting (enemyRef enemy) (discardUpgrade (upgradeId attrs) >> confuse enemy attrs) | enemy <- enemies])
      pure u
    _ -> KimoyoBeadsShuri <$> runMessage msg attrs

instance HasAbilities KimoyoBeadsShuri where
  getAbilities _ = []

instance IsUpgrade KimoyoBeadsShuri where
  toUpgradeAttrs (KimoyoBeadsShuri attrs) = attrs
  upgradeSpecialCanResolve (KimoyoBeadsShuri _attrs) = (||) <$> selectAny ThwartableScheme <*> selectAny EnemyCanBeConfused
