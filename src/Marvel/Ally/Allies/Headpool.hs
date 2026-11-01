module Marvel.Ally.Allies.Headpool where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Stats ()

headpool :: AllyCard Headpool
headpool = ally Headpool Cards.headpool (Thw 2, 0) (Atk 1, 0) (HP 1)

newtype Headpool = Headpool (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Headpool where
  getAbilities a@(Headpool attrs) =
    [limitedWindowAbility a 1
      (W.AllyAttackDamagedEnemy (AllyWithId $ allyId attrs) (MinionEnemy <> EnemyInPlay))
      Response OwnsThis NoCost (runAbility a 1)]


instance RunMessage Headpool where
  runMessage msg e@(Headpool attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ (headMay [mid | W.AllyAttackDamaged _ (EnemyMinionId mid) _ <- windows]) $ \mid -> do
        alive <- selectAny (MinionWithId mid)
        when alive do
          enemies <- selectList (AnyEnemy <> NotEnemy (EnemyWithId $ EnemyMinionId mid))
          chooseOne you [targeting (enemyRef enemy) (minionAttacksEnemy mid enemy) | enemy <- enemies]
      pure e
    _ -> Headpool <$> runMessage msg attrs
