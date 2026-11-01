module Marvel.Ally.Allies.PinpointQureshiGupta where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers ()
import Marvel.Message.Lifted ()
import Marvel.Modifier ()
import Marvel.Ref ()
import Marvel.Window qualified as W
import Marvel.Card ()

pinpointQureshiGupta :: AllyCard PinpointQureshiGupta
pinpointQureshiGupta = ally PinpointQureshiGupta Cards.pinpointQureshiGupta (Thw 1, 1) (Atk 2, 2) (HP 2)

newtype PinpointQureshiGupta = PinpointQureshiGupta (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities PinpointQureshiGupta where
  getAbilities a = [limitedWindowAbility a 1 W.PlayerCardWouldBeDiscardedFromPlay HeroInterrupt OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage PinpointQureshiGupta where
  runMessage msg a@(PinpointQureshiGupta attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [card | W.WouldBeDiscardedFromPlay (Left card) <- windows] \card ->
        for_ (pcOwner card) \owner -> insteadOf (== DiscardedCard (PlayerCard card)) (shuffleIntoDeck owner [card])
      pure a
    _ -> PinpointQureshiGupta <$> runMessage msg attrs
