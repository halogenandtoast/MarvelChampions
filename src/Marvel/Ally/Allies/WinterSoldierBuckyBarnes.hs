module Marvel.Ally.Allies.WinterSoldierBuckyBarnes (
  winterSoldierBuckyBarnes,
  WinterSoldierBuckyBarnes (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Id (unAllyId)
import Marvel.Matchers
import Marvel.Query
import Marvel.Trait

winterSoldierBuckyBarnes :: AllyCard WinterSoldierBuckyBarnes
winterSoldierBuckyBarnes =
  ally
    WinterSoldierBuckyBarnes
    Cards.winterSoldierBuckyBarnes
    (Thw 2, 1)
    (Atk 2, 1)
    (HP 4)

newtype WinterSoldierBuckyBarnes = WinterSoldierBuckyBarnes (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | In hand, he reduces the cost to play himself by 1 for each Preparation card you
-- control. ponytail: Preparation cards are upgrades so far
instance HasModifiersFor WinterSoldierBuckyBarnes where
  getModifiersFor _ (CardIdRef cid) (WinterSoldierBuckyBarnes attrs)
    | cid == CardId (unAllyId $ allyId attrs) = do
        n <- selectListCount (UpgradeWithTrait Preparation <> UpgradeControlledBy (IdentityWithId $ allyController attrs))
        pure [ResourceCostReduction n]
  getModifiersFor _ _ _ = pure []

instance RunMessage WinterSoldierBuckyBarnes where
  runMessage msg (WinterSoldierBuckyBarnes attrs) = WinterSoldierBuckyBarnes <$> runMessage msg attrs
