module Marvel.Ally.Allies.BoomBoom where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

boomBoom :: AllyCard BoomBoom
boomBoom = ally BoomBoom Cards.boomBoom (Thw 1, 1) (Atk 1, 1) (HP 2)

newtype BoomBoom = BoomBoom (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities BoomBoom where
  getAbilities a = [ability a 1 Action OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage BoomBoom where
  runMessage msg a@(BoomBoom attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      placeCounters attrs 1
      cardSpecific attrs "detonate" you
      pure a
    CardSpecific (isTarget attrs -> True) "detonate" (fromJSON -> Success you) -> do
      n <- getCountersOn attrs
      chooseOne you
        [ labeled "$label.detonateBoomBoom" do
            removeFromPlay attrs
            enemies <- selectList DamageableEnemy
            dealDamageToEach enemies attrs (toDamage n FromAbility)
        , labeled "$label.done" (pure ())
        ]
      pure a
    _ -> BoomBoom <$> runMessage msg attrs
