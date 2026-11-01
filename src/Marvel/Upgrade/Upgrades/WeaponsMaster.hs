module Marvel.Upgrade.Upgrades.WeaponsMaster (weaponsMaster, WeaponsMaster (..)) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost.Types ()
import Marvel.Count ()
import Marvel.Criteria ()
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hero.Heroes.NebulaHero (techniqueResolved)
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

weaponsMaster :: UpgradeCard WeaponsMaster
weaponsMaster = upgrade WeaponsMaster Cards.weaponsMaster

newtype WeaponsMaster = WeaponsMaster (Attrs Upgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade WeaponsMaster where
  upgradeSpecialCanResolve (WeaponsMaster attrs) = do
    let ident = upgradeController attrs
    canAttack <- selectAny (IdentityWithId ident <> IdentityCanAttack)
    stunned <- selectAny (IdentityWithId ident <> StunnedIdentity)
    targets <- weaponsMasterTargets attrs
    pure $ canAttack && (stunned || not (null targets))

weaponsMasterTargets :: (HasGame m) => Attrs Upgrade -> m [EnemyId]
weaponsMasterTargets attrs = asYou (upgradeController attrs) do
  enemies <- selectList (AttackableBy $ IdentityWithId $ upgradeController attrs)
  sourceModifiers <- getModifiers attrs
  filterM (\enemy -> do
    modifiers <- getModifiers (enemyRef enemy)
    let amount = 4 + sum [n | DamageDealtModifier n <- sourceModifiers]
        reduced = subtractNatural (sum [n | AttackDamageReduction n <- modifiers]) amount
        capped = foldr min reduced [n | AttackDamageCap n <- modifiers]
    pure $ capped > 0 && CannotTakeDamage `notElem` modifiers && CannotTakeDamageFromPlayerCards `notElem` modifiers) enemies

instance HasAbilities WeaponsMaster where
  getAbilities _ = []

instance HasModifiersFor WeaponsMaster where
  getModifiersFor _ (IdentityRef ident) (WeaponsMaster attrs)
    | ident == upgradeController attrs = do
        hero <- selectAny (IdentityWithId ident <> HeroIdentity)
        pure $ if hero then [KeywordModifier $ Retaliate 1] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage WeaponsMaster where
  runMessage msg u@(WeaponsMaster attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      canAttack <- selectAny (IdentityWithId ident <> IdentityCanAttack)
      if not canAttack then pure u else attackGuard ident u do
        eligible <- weaponsMasterTargets attrs
        unless (null eligible) $ chooseOne ident
          [targeting (enemyRef e) do
            dealDamage e attrs (toDamage 4 $ FromPlayerAttack ident)
            techniqueResolved ident (upgradeId attrs)
          | e <- eligible]
        pure u
    _ -> WeaponsMaster <$> runMessage msg attrs
