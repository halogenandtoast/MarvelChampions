{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Ally.Allies.AosP50108b where

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

aosP50108b :: AllyCard AosP50108b
aosP50108b = ally AosP50108b Cards.aosP50108b (Thw 1, 1) (Atk 3, 1) (HP 7)

newtype AosP50108b = AosP50108b (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor AosP50108b where
  getModifiersFor _ target (AosP50108b attrs) = pure $ [NotCountedAgainstAllyLimit | target == toRef attrs]

instance RunMessage AosP50108b where
  runMessage msg (AosP50108b attrs) = runQueueT $ AosP50108b <$> runMessage msg attrs
