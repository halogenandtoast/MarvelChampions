{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Minion.Minions.Fw53030 where
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

fw53030 :: MinionCard Fw53030
fw53030 = minion Fw53030 Cards.fw53030 (Sch 2) (Atk 2) (HP 5)

newtype Fw53030 = Fw53030 (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw53030 where
  getAbilities a@(Fw53030 attrs) =
    [limitedWindowAbility a 1 (W.EnemyActivates W.After (EnemyWithId $ EnemyMinionId $ minionId attrs) AnyIdentity) ForcedResponse (NoCriteria) (NoCost) (runAbility a 1)]

instance RunMessage Fw53030 where
  runMessage msg a@(Fw53030 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      discardTopOfEncounterDeck 5
      pure a
    _ -> Fw53030 <$> runMessage msg attrs
