{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Ally.Allies.AosP50105b where

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

aosP50105b :: AllyCard AosP50105b
aosP50105b = ally AosP50105b Cards.aosP50105b (Thw 2, 1) (Atk 1, 1) (HP 5)

newtype AosP50105b = AosP50105b (Attrs Ally)
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50105b where
  getAbilities (AosP50105b attrs) =
    [limitedWindowAbility attrs 1 (W.AllyThwarted W.After (AllyWithId $ allyId attrs) AnyScheme) Response OwnsThis NoCost (runAbility attrs 1)]

instance HasModifiersFor AosP50105b where
  getModifiersFor _ target (AosP50105b attrs) = pure $ [NotCountedAgainstAllyLimit | target == toRef attrs]

instance RunMessage AosP50105b where
  runMessage msg card@(AosP50105b attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      let thwarted = [scheme | W.AllyThwart _ scheme <- windows]
      schemes <- filter (`notElem` thwarted) <$> selectList ThwartableScheme
      chooseOne you [targeting (schemeRef scheme) (thwartScheme scheme attrs 1) | scheme <- schemes]
      pure card
    _ -> AosP50105b <$> runMessage msg attrs
