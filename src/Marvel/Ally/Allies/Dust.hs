module Marvel.Ally.Allies.Dust (dust, Dust (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.Stats
import Marvel.Ref
import Marvel.Window qualified as W

dust :: AllyCard Dust
dust = ally (Dust . (`With` Meta False)) Cards.dust (Thw 1, 1) (Atk 1, 1) (HP 3)

newtype Meta = Meta {attackingAll :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Dust = Dust (Attrs Ally `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsAlly Dust where
  toAllyAttrs (Dust (attrs `With` _)) = attrs

instance IsRef Dust where
  toRef = toRef . toAllyAttrs

instance HasModifiersFor Dust where
  getModifiersFor _ (AllyRef aid) (Dust (attrs `With` meta))
    | aid == allyId attrs && attackingAll meta = pure [AttackConsequentialDamageModifier 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities Dust where
  getAbilities a@(Dust (_ `With` meta)) =
    [limitedWindowAbility a 1 (W.AllyAttacked W.When (AllyWithId $ allyId $ toAllyAttrs a) MinionEnemy)
      Interrupt OwnsThis NoCost (runAbility a 1)
    | not (attackingAll meta)
    ]

instance RunMessage Dust where
  runMessage msg a@(Dust (attrs `With` meta)) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      minions <- selectList AnyMinion
      resolveAllyAttackAgainstAll (allyId attrs) (map EnemyMinionId minions)
      pure . Dust $ attrs `With` Meta True
    AllyMessage aid AllyAttacked | aid == allyId attrs -> do
      attrs' <- runMessage msg attrs
      pure . Dust $ attrs' `With` Meta False
    AllyMessage aid consequence | aid == allyId attrs, (case consequence of AllyAttackConsequential -> True; AllyAttackConsequentialAgainst _ -> True; _ -> False) -> do
      -- The game's current copy still has the modifier while the base handler
      -- calculates this use's single consequential-damage payment.
      attrs' <- runMessage msg attrs
      pure . Dust $ attrs' `With` Meta False
    AllyMessage aid (AllyAttackMultipleFinished []) | aid == allyId attrs -> do
      attrs' <- runMessage msg attrs
      pure . Dust $ attrs' `With` Meta False
    _ -> Dust . (`With` meta) <$> runMessage msg attrs
