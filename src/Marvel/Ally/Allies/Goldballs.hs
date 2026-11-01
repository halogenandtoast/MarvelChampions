module Marvel.Ally.Allies.Goldballs where

import Marvel.Prelude
import Marvel.Payment
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Window qualified as W

goldballs :: AllyCard Goldballs
goldballs = ally Goldballs Cards.goldballs (Thw 1, 1) (Atk 1, 1) (HP 3)

newtype Goldballs = Goldballs (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Goldballs where
  getAbilities a@(Goldballs attrs) =
    [limitedWindowAbility a 1 (W.AllyAttacked W.When (AllyWithId $ allyId attrs) AnyEnemy)
      Interrupt OwnsThis (DiscardTopOfDeckCardsCost 0 3) (runAbility a 1)]

instance RunMessage Goldballs where
  runMessage msg a@(Goldballs attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ payment -> do
      addToAllyBasicPower (allyId attrs) (fromIntegral $ length $ paymentDiscards payment)
      pure a
    _ -> Goldballs <$> runMessage msg attrs
