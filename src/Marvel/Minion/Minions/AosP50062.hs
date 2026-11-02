{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Minion.Minions.AosP50062 where

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

aosP50062 :: MinionCard AosP50062
aosP50062 = minion AosP50062 Cards.aosP50062 (Sch 0) (Atk 2) (HP 4)

newtype AosP50062 = AosP50062 (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50062 where
  getAbilities (AosP50062 a) = [windowAbility a 1 (W.MinionHasSchemed W.After $ MinionWithId $ minionId a) ForcedResponse NoCost (runAbility a 1)]

instance RunMessage AosP50062 where
  runMessage msg e@(AosP50062 a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> e <$ damageIdentity (minionEngagedIdentity a) a (toDamage 1 FromAbility)
    _ -> AosP50062 <$> runMessage msg a
