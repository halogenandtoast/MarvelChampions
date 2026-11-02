{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Minion.Minions.Fw54034 where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Attack (attackCharacter)
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

import Marvel.FalconWinter.Helpers
fw54034 :: MinionCard Fw54034
fw54034 = minion Fw54034 Cards.fw54034 (Sch 1) (Atk 2) (HP 16)

newtype Fw54034 = Fw54034 (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw54034 where
  getAbilities a@(Fw54034 attrs) =
    [limitedWindowAbility a 1 (W.EnemyAttackedAnyTarget W.After $ EnemyWithId $ EnemyMinionId $ minionId attrs) ForcedResponse (NoCriteria) (NoCost) (runAbility a 1)]

instance RunMessage Fw54034 where
  runMessage msg a@(Fw54034 attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      attack <- getEnemyAttack $ EnemyMinionId $ minionId attrs
      let targets = case attack of
            Just current -> [attackCharacter current]
            Nothing -> [toCharacter target | W.EnemyAttackOnEnemy _ target <- windows]
      for_ targets $ \target -> whenM (selectAny $ CharacterWithId target) $ findIce target
      pure a
    _ -> Fw54034 <$> runMessage msg attrs
