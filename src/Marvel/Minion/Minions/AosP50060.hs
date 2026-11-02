{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Minion.Minions.AosP50060 where

import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Window qualified as W
import Marvel.Matchers
import Marvel.Damage
import Marvel.Cost
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

aosP50060 :: MinionCard AosP50060
aosP50060 = minion AosP50060 Cards.aosP50060 (Sch 2) (Atk 2) (HP 6)

newtype AosP50060 = AosP50060 (Attrs Minion)
  deriving anyclass IsMinion
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor AosP50060 where
  getModifiersFor _ target (AosP50060 a) = pure [CanHaveAnyNumberOfToughStatusCards | isTarget a target]

instance HasAbilities AosP50060 where
  getAbilities (AosP50060 a) = [windowAbility a 1 (W.EnemyTookDamage (EnemyWithId $ EnemyMinionId $ minionId a) AnyDamage) ForcedResponse NoCost (runAbility a 1)]

instance RunMessage AosP50060 where
  runMessage msg e@(AosP50060 a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> e <$ giveStatus (minionId a) ToughStatus
    _ -> AosP50060 <$> runMessage msg a
