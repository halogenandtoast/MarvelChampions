{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Ally.Allies.Fw54032 where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Ally.Types
import Marvel.Ally.Runner ()

import Marvel.FalconWinter.Helpers
fw54032 :: AllyCard Fw54032
fw54032 = ally Fw54032 Cards.fw54032 (Thw 2, 1) (Atk 1, 1) (HP 3)

newtype Fw54032 = Fw54032 (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw54032 where
  getAbilities a@(Fw54032 attrs) =
    [limitedWindowAbility a 1 (W.ResolvesAbility W.After (IdentityWithId $ allyController attrs) $ CardWithTrait T.Preparation <> CardWithPrintedCostAtLeast 1) Response (OwnsThis) (NoCost) (runAbility a 1) | allyDamage attrs > 0]

instance RunMessage Fw54032 where
  runMessage msg a@(Fw54032 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      let costs = [fromIntegral n | W.ResolvedAbility _ def <- windows, Just n <- [cdCost def], n > 0]
      for_ costs $ heal (allyId attrs)
      pure a
    _ -> Fw54032 <$> runMessage msg attrs
