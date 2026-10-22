module Marvel.Ally.Allies.SheHulkJenniferWalters (
  sheHulkJenniferWalters,
  SheHulkJenniferWalters (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

sheHulkJenniferWalters :: AllyCard SheHulkJenniferWalters
sheHulkJenniferWalters = ally SheHulkJenniferWalters Cards.sheHulkJenniferWalters (Thw 2, 2) (Atk 1, 1) (HP 4)

newtype SheHulkJenniferWalters = SheHulkJenniferWalters (Attrs Ally)
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef, HasController)

-- | "She-Hulk gets +1 ATK for each damage token here."
instance HasModifiersFor SheHulkJenniferWalters where
  getModifiersFor _ (AllyRef aid) (SheHulkJenniferWalters a)
    | aid == allyId a = pure [AttackModifier (allyDamage a) | allyDamage a > 0]
  getModifiersFor _ _ _ = pure []

instance HasAbilities SheHulkJenniferWalters

instance RunMessage SheHulkJenniferWalters where
  runMessage msg (SheHulkJenniferWalters attrs) = SheHulkJenniferWalters <$> runMessage msg attrs
