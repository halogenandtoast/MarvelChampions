{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Ally.Allies.Fw53035 where
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

fw53035 :: AllyCard Fw53035
fw53035 = ally Fw53035 Cards.fw53035 (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype Fw53035 = Fw53035 (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw53035 where
  getAbilities a@(Fw53035 attrs) =
    [limitedWindowAbility a 1 (W.EnemyDefeated W.After AnyEnemy (AttackFromAlly $ AllyWithId $ allyId attrs)) Response (OwnsThis <> SchemeExists ThwartableScheme) (NoCost) (runAbility a 1)]

instance RunMessage Fw53035 where
  runMessage msg a@(Fw53035 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      removeThreat you attrs 2 ThwartableScheme
      pure a
    _ -> Fw53035 <$> runMessage msg attrs
