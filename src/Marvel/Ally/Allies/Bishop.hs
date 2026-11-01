module Marvel.Ally.Allies.Bishop where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Payment ()
import Marvel.Window qualified as W

bishop :: AllyCard Bishop
bishop = ally Bishop Cards.bishop (Thw 2, 1) (Atk 0, 2) (HP 3)

newtype Bishop = Bishop (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Bishop where
  getAbilities a@(Bishop attrs) =
    [ limitedWindowAbility a 1
        (W.EnemyAttacked W.After AnyEnemy $ IdentityWithId $ allyController attrs)
        Response OwnsThis NoCost (runAbility a 1)
    ] <>
    [ limitedWindowAbility a 2
        (W.AllyAttacked W.When (AllyWithId $ allyId attrs) AnyEnemy)
        Interrupt OwnsThis (ExactCountersCost $ allyCounters attrs) (runAbility a 2)
    | allyCounters attrs > 0
    ]

instance RunMessage Bishop where
  runMessage msg a@(Bishop attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> a <$ placeCounters attrs 1
    RanAbility _ (isTarget attrs -> True) 2 _ payment -> do
      let spent = sum [n | CountersPayment n <- payments payment]
      raiseAllyAttack (allyId attrs) (min 6 $ 2 * spent)
      pure a
    _ -> Bishop <$> runMessage msg attrs
