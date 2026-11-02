{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Minion.Minions.Fw54028 where
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

fw54028 :: MinionCard Fw54028
fw54028 = minion Fw54028 Cards.fw54028 (Sch 2) (Atk 2) (HP 5)

newtype Fw54028 = Fw54028 (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw54028 where
  getAbilities a@(Fw54028 attrs) =
    [limitedWindowAbility a 1 (W.CharacterDefeatedByEnemyAttack W.After $ EnemyWithId $ EnemyMinionId $ minionId attrs) ForcedResponse (NoCriteria) (NoCost) (runAbility a 1)]

instance RunMessage Fw54028 where
  runMessage msg a@(Fw54028 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      when (any (\case W.EnemyAttackDefeatedCharacter _ (AllyCharacter _) -> True; W.DefeatedAlly _ (Just _) -> True; _ -> False) windows) $ do
        schemes <- selectList MainScheme
        for_ schemes $ \scheme -> placeThreatOn attrs scheme 2
      pure a
    _ -> Fw54028 <$> runMessage msg attrs
