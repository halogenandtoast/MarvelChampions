{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Ally.Allies.AosP50002 where

import Marvel.Prelude
import Marvel.Question
import Marvel.Id
import Marvel.Projection
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Payment
import Marvel.Query
import Marvel.Resource.Types qualified as R
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.AgentsOfShield.PlayerHelpers

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

aosP50002 :: AllyCard AosP50002
aosP50002 = ally AosP50002 Cards.aosP50002 (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype AosP50002 = AosP50002 (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50002 where
  getAbilities (AosP50002 attrs) =
    [limitedWindowAbility attrs index window Response OwnsThis NoCost (runAbility attrs 1)
    | (index,window) <- [(1,W.AllyAttacked W.After (AllyWithId $ allyId attrs) AnyEnemy),(2,W.AllyThwarted W.After (AllyWithId $ allyId attrs) AnyScheme),(3,W.AllyDefendsAgainst W.After (AllyWithId $ allyId attrs) AnyEnemy)]]

instance RunMessage AosP50002 where
  runMessage msg card@(AosP50002 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      chooseShieldCounter you attrs
      pure card
    _ -> AosP50002 <$> runMessage msg attrs
