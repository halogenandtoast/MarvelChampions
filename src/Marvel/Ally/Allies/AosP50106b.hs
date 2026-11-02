{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Ally.Allies.AosP50106b where

import Marvel.Prelude
import Marvel.Question
import Marvel.Id
import Marvel.Projection
import Marvel.Card
import Marvel.Cost
import Marvel.Keyword
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

aosP50106b :: AllyCard AosP50106b
aosP50106b = ally AosP50106b Cards.aosP50106b (Thw 1, 1) (Atk 1, 1) (HP 5)

newtype AosP50106b = AosP50106b (Attrs Ally)
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50106b where
  getAbilities (AosP50106b attrs) =
    [limitedWindowAbility attrs 1 (W.AllyThwarted W.After (AllyWithId $ allyId attrs) AnyScheme) Response OwnsThis NoCost (runAbility attrs 1)]

instance HasModifiersFor AosP50106b where
  getModifiersFor _ target (AosP50106b attrs) = pure $ [NotCountedAgainstAllyLimit | target == toRef attrs]

instance RunMessage AosP50106b where
  runMessage msg card@(AosP50106b attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cells <- selectList (EnvironmentWithTitle "Holding Cell")
      chooseOne you [targeting eid (removeCounters eid 1) | eid <- cells]
      pure card
    _ -> AosP50106b <$> runMessage msg attrs
