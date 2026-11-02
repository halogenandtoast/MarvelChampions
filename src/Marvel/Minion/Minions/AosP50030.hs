{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Minion.Minions.AosP50030 where

import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Matchers
import Marvel.Window qualified as W
import Marvel.Cost
import Marvel.AgentsOfShield.NemesisHelpers
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

aosP50030 :: MinionCard AosP50030
aosP50030 = minion AosP50030 Cards.aosP50030 (Sch 2) (Atk 1) (HP 6)

newtype AosP50030 = AosP50030 (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50030 where
  getAbilities (AosP50030 a) = [windowAbility a 1 (W.EnemyActivates W.After (EnemyWithId $ EnemyMinionId $ minionId a) AnyIdentity) ForcedResponse NoCost (runAbility a 1)]

instance RunMessage AosP50030 where
  runMessage msg e@(AosP50030 a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      for_ [you | W.EnemyActivation _ you <- windows] $ \you -> removeSupportCounter you >> addControlledMinion you
      pure e
    _ -> AosP50030 <$> runMessage msg a
