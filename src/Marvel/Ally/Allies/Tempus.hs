module Marvel.Ally.Allies.Tempus where

import Marvel.Prelude
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

tempus :: AllyCard Tempus
tempus = ally Tempus Cards.tempus (Thw 1, 1) (Atk 1, 1) (HP 2)

newtype Tempus = Tempus (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Tempus where
  getAbilities a =
    [limitedWindowAbility a 1 (W.VillainSchemed W.When AnyVillain) Interrupt OwnsThis
      (DiscardCost $ toRef a) (runAbility a 1)]

instance RunMessage Tempus where
  runMessage msg a@(Tempus attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ [villain | W.Schemed villain <- windows] cancelEnemyActivation
      dealEncounterCard you
      pure a
    _ -> Tempus <$> runMessage msg attrs
