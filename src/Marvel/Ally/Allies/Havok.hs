module Marvel.Ally.Allies.Havok where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Boost
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Ref
import Marvel.Window qualified as W

havok :: AllyCard Havok
havok = ally (Havok . (`With` 0)) Cards.havok (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype Havok = Havok (Attrs Ally `With` Natural)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsAlly Havok where
  toAllyAttrs (Havok (a `With` _)) = a
instance IsRef Havok where
  toRef = toRef . toAllyAttrs

instance HasAbilities Havok where
  getAbilities a =
    [limitedWindowAbility a 1 (W.AllyAttacked W.When (AllyWithId $ allyId $ toAllyAttrs a) AnyEnemy)
      ForcedInterrupt OwnsThis NoCost (runAbility a 1)]

instance HasModifiersFor Havok where
  getModifiersFor _ (AllyRef aid) (Havok (a `With` bonus))
    | aid == allyId a = pure [AttackModifier bonus, AttackConsequentialDamageModifier bonus]
  getModifiersFor _ _ _ = pure []

instance RunMessage Havok where
  runMessage msg a@(Havok (attrs `With` bonus)) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      discardTopOfEncounterDeckFor 1 a
      pure a
    WithDiscarded (isTarget a -> True) _ cards -> do
      let n = sum [boostCount (cdBoostIcons $ getCardDef card) | card <- cards]
      raiseAllyAttack (allyId attrs) n
      pure $ Havok (attrs `With` n)
    AllyMessage aid action | aid == allyId attrs, (case action of AllyAttacked -> True; AllyAttackConsequential -> True; AllyAttackConsequentialAgainst _ -> True; _ -> False) -> do
      attrs' <- runMessage msg attrs
      pure $ Havok (attrs' `With` 0)
    _ -> Havok . (`With` bonus) <$> runMessage msg attrs
