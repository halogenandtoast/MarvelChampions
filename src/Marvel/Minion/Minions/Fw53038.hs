{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Minion.Minions.Fw53038 where
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

fw53038 :: MinionCard Fw53038
fw53038 = minion Fw53038 Cards.fw53038 (Sch 2) (Atk 1) (HP 15)

newtype Fw53038 = Fw53038 (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw53038 where
  getAbilities a@(Fw53038 attrs) =
    [limitedWindowAbility a 1 (W.NonScenarioAttachmentWouldAttachToVillain T.Tech) ForcedInterrupt (NoCriteria) (NoCost) (runAbility a 1)]

instance RunMessage Fw53038 where
  runMessage msg a@(Fw53038 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      for_ [aid | W.AttachmentAttachingToEnemy aid _ <- windows] $ \aid -> redirectAttachmentTo aid (minionId attrs)
      pure a
    _ -> Fw53038 <$> runMessage msg attrs
