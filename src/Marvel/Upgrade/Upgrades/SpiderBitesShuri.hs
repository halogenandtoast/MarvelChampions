module Marvel.Upgrade.Upgrades.SpiderBitesShuri where
import Marvel.Prelude
import Marvel.Query
import Marvel.Id
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage
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

spiderBitesShuri :: UpgradeCard SpiderBitesShuri
spiderBitesShuri = upgrade SpiderBitesShuri Cards.spiderBitesShuri
newtype SpiderBitesShuri = SpiderBitesShuri (Attrs Upgrade)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage SpiderBitesShuri where
  runMessage msg u@(SpiderBitesShuri attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      players <- selectList AnyIdentity
      chooseOne you [targeting player (cardSpecific attrs "player" (you,player)) | player <- players]
      pure u
    CardSpecific (isTarget attrs -> True) "player" (fromJSON -> Success (you :: IdentityId,player :: IdentityId)) -> do
      minions <- selectList (MinionEngagedWith (IdentityWithId player))
      villains <- selectList AnyVillain
      if null villains
        then cardSpecific attrs "enemies" (you,map EnemyMinionId minions)
        else withTheVillain you $ \villain -> cardSpecific attrs "enemies" (you,EnemyVillainId villain : map EnemyMinionId minions)
      pure u
    CardSpecific (isTarget attrs -> True) "enemies" (fromJSON -> Success (you :: IdentityId,enemies :: [EnemyId])) -> do
      dealDamageToEach enemies attrs (toDamage 1 FromAbility)
      cardSpecific attrs "stun" (you,enemies)
      pure u
    CardSpecific (isTarget attrs -> True) "stun" (fromJSON -> Success (you,enemies :: [EnemyId])) -> do
      chooseOne you [labeled "$label.done" (pure ()), targeting attrs (discardUpgrade (upgradeId attrs) >> traverse_ (`stun` attrs) enemies)]
      pure u
    _ -> SpiderBitesShuri <$> runMessage msg attrs

instance HasAbilities SpiderBitesShuri where
  getAbilities _ = []

instance IsUpgrade SpiderBitesShuri where
  toUpgradeAttrs (SpiderBitesShuri attrs) = attrs
  upgradeSpecialCanResolve (SpiderBitesShuri _attrs) = selectAny AnyEnemy
