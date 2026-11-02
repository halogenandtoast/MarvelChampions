{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Minion.Minions.Fw53032 where
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
import Marvel.Minion.Types
import Marvel.Minion.Runner ()

import Marvel.Attack (sourceEnemy)
fw53032 :: MinionCard Fw53032
fw53032 = minion Fw53032 Cards.fw53032 (Sch 1) (Atk 2) (HP 3)

newtype Fw53032 = Fw53032 (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw53032 where
  getAbilities a@(Fw53032 attrs) =
    []

instance RunMessage Fw53032 where
  runMessage msg a@(Fw53032 attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      dealBoost enemy
      pure a
    _ -> Fw53032 <$> runMessage msg attrs
