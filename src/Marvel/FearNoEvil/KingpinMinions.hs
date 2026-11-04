module Marvel.FearNoEvil.KingpinMinions where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Types qualified as A
import Marvel.Ally.Runner ()
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Game.Source
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Types
import Marvel.Minion.Runner ()
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Window qualified as W

newtype KingpinMinion = KingpinMinion (Attrs Minion)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
instance IsMinion KingpinMinion where
  minionDisplayCounters (KingpinMinion a) = minionCounters a
kingpinMinions :: [MinionCard KingpinMinion]
kingpinMinions = [minion (\a -> KingpinMinion a {minionCounters=counters}) def (Sch sch) (Atk atk) (HP hp) | (def,sch,atk,hp,counters) <-
  [(Cards.street60169,1,1,6,0),(Cards.street60170,1,1,6,0),(Cards.street60171,2,2,5,1),(Cards.street60172,1,1,4,2)]]
instance HasModifiersFor KingpinMinion where
  getModifiersFor _ target (KingpinMinion a) = pure [modifier | target == toRef a,toCardCode a == "60172",modifier <- [SchemeModifier $ minionCounters a,AttackModifier $ minionCounters a]]
instance HasAbilities KingpinMinion where
  getAbilities (KingpinMinion a) = case toCardCode a of
    "60169" -> [ability a 1 HeroAction NoCriteria (PrintedResourceCost [Mental]) $ runAbility a 1]
    "60170" -> [windowAbility a 2 (W.EnemyAttacked W.When (EnemyWithId $ toEnemyId a) AnyIdentity) ForcedInterrupt NoCost (runAbility a 2)]
    "60171" -> [windowAbility a 3 (W.EnemyWouldBeDealtDamageBy (EnemyWithId $ toEnemyId a) DamageFromAnAttack) ForcedInterrupt NoCost (runAbility a 3)]
    _ -> [windowAbility a 4 (W.EnemyWouldTakeDamage $ EnemyWithId $ toEnemyId a) ForcedInterrupt NoCost (runAbility a 4) | minionCounters a > 0]
instance RunMessage KingpinMinion where
  runMessage msg e@(KingpinMinion a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      placeCharacterThreat (minionId a) 1
      cardSpecific a "blackCatControl" you
      pure e
    CardSpecific (isTarget a -> True) "blackCatControl" (fromJSON -> Success you) -> do
      threat <- getCharacterThreat $ MinionCharacter $ minionId a
      when (threat == 4) $ takeControlOfMinionWithConsequential you (minionId a) 2
      pure e
    RanAbility _ (isTarget a -> True) 2 _ _ -> do
      you <- getBoostedPlayer $ toEnemyId a
      damageIdentity you a $ toDamage 1 FromAbility
      pure e
    RanAbility _ (isTarget a -> True) 3 windows _ -> do
      for_ [(source,damage) | W.EnemyDamageWouldBeDealt enemy source damage <- windows,enemy == toEnemyId a] $ \(source,damage) ->
        if minionCounters a == 0 then placeCounters a 1 else do
          removeCounters a 1
          cancelEnemyDamage (toEnemyId a)
          attacker <- threatSourceCharacter source
          for_ attacker $ \character -> damageCharacter character a $ toDamage (damageAmount damage) FromAbility
      pure e
    RanAbility _ (isTarget a -> True) 4 _ _ -> cancelEnemyDamage (toEnemyId a) >> removeCounters a 1 >> pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target,toCardCode a == "60170" -> do
      you <- getBoostedPlayer enemy
      damageIdentity you a $ toDamage 1 FromAbility
      pure e
    _ -> KingpinMinion <$> runMessage msg a

-- Black Cat keeps her text, including Villainous, when she changes allegiance.
newtype BlackCatAlly = BlackCatAlly (Attrs A.Ally)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass (A.IsAlly,HasModifiersFor)
blackCatAlly :: A.AllyCard BlackCatAlly
blackCatAlly = A.ally BlackCatAlly Cards.street60169 (Thw 1,2) (Atk 1,2) (HP 6)
instance HasAbilities BlackCatAlly where
  getAbilities (BlackCatAlly a) = [ability a 1 HeroAction NoCriteria (PrintedResourceCost [Mental]) $ runAbility a 1]
instance RunMessage BlackCatAlly where
  runMessage msg e@(BlackCatAlly a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      placeCharacterThreat (A.allyId a) 1
      cardSpecific GameRef "blackCatCheckAlly" (A.allyId a)
      pure e
    _ -> BlackCatAlly <$> runMessage msg a
