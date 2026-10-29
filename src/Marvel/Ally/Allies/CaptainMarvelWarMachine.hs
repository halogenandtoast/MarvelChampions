module Marvel.Ally.Allies.CaptainMarvelWarMachine (captainMarvelWarMachine, CaptainMarvelWarMachine (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Resource
import Marvel.Window

captainMarvelWarMachine :: AllyCard CaptainMarvelWarMachine
captainMarvelWarMachine = ally CaptainMarvelWarMachine Cards.captainMarvelWarMachine (Thw 2, 1) (Atk 3, 2) (HP 4)

newtype CaptainMarvelWarMachine = CaptainMarvelWarMachine (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities CaptainMarvelWarMachine where
  getAbilities a = [limitedWindowAbility a 1 EntersPlayThis Response OwnsThis NoCost (runAbility a 1)]

instance RunMessage CaptainMarvelWarMachine where
  runMessage msg a@(CaptainMarvelWarMachine attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      discardFromDeck ident 4 attrs
      pure a
    WithDiscarded (isTarget attrs -> True) _ (onlyPlayerCards -> cards) -> do
      let energy = count (== Energy) (concatMap (printedResources . getCardDef) cards)
      when (energy > 0) do
        enemies <- selectList AnyEnemy
        valid <- filterM (\enemy -> do
          damageable <- not <$> cannotTakeDamageFrom (enemyRef enemy) (toSource attrs)
          stalwart <- isStalwartEnemy (enemyRef enemy)
          stunned <- enemyMatches StunnedEnemy enemy
          pure $ damageable || (energy > 1 && not stalwart && not stunned)) enemies
        unless (null valid) $ chooseOne (allyController attrs)
          [ targeting (enemyRef enemy) do
              dealDamage enemy attrs (toDamage 3 FromAbility)
              when (energy > 1) $ cardSpecific attrs "stun" enemy
          | enemy <- valid ]
      pure a
    CardSpecific (isTarget attrs -> True) "stun" (fromJSON -> Success (enemy :: EnemyId)) -> do
      alive <- selectAny (EnemyWithId enemy)
      when alive $ stun enemy attrs
      pure a
    _ -> CaptainMarvelWarMachine <$> runMessage msg attrs
