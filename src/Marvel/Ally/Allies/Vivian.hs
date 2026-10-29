module Marvel.Ally.Allies.Vivian where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Query
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Stats
import Marvel.Hp
vivian :: AllyCard Vivian
vivian = ally Vivian Cards.vivian (Thw 1, 1) (Atk 1, 1) (HP 2)

newtype Vivian = Vivian (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Vivian where
  getModifiersFor _ (AllyRef aid) (Vivian a) | aid == allyId a = do
    dense <- isInMassForm (allyController a) Upgrades.dense
    intangible <- isInMassForm (allyController a) Upgrades.intangible
    pure $ [AttackModifier 2 | dense] <> [ThwartModifier 2 | intangible]
  getModifiersFor _ _ _ = pure []

instance RunMessage Vivian where
  runMessage msg (Vivian attrs) = Vivian <$> runMessage msg attrs
