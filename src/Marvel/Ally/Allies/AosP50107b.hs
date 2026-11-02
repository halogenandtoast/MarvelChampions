{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Ally.Allies.AosP50107b where

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

aosP50107b :: AllyCard AosP50107b
aosP50107b = ally AosP50107b Cards.aosP50107b (Thw 2, 1) (Atk 2, 1) (HP 5)

newtype AosP50107b = AosP50107b (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor AosP50107b where
  getModifiersFor _ target (AosP50107b attrs) = pure $ [NotCountedAgainstAllyLimit | target == toRef attrs] <> [KeywordModifier Ranged | target == toRef attrs] <> [KeywordModifier Overkill | target == toRef attrs]

instance RunMessage AosP50107b where
  runMessage msg (AosP50107b attrs) = runQueueT $ AosP50107b <$> runMessage msg attrs
