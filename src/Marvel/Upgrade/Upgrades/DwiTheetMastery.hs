module Marvel.Upgrade.Upgrades.DwiTheetMastery (
  dwiTheetMastery,
  DwiTheetMastery (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

dwiTheetMastery :: UpgradeCard DwiTheetMastery
dwiTheetMastery = upgrade DwiTheetMastery Cards.dwiTheetMastery

newtype DwiTheetMastery = DwiTheetMastery (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Hero Response: After Drax makes a basic attack, draw 1 card."
instance HasAbilities DwiTheetMastery where
  getAbilities a@(DwiTheetMastery attrs) =
    [ limitedWindowAbility
        a
        1
        (MakesBasicAttack After $ IdentityWithId $ upgradeController attrs)
        HeroResponse
        OwnsThis
        NoCost
        (runAbility a 1)
    ]

instance RunMessage DwiTheetMastery where
  runMessage msg u@(DwiTheetMastery attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      drawCards ident 1
      pure u
    _ -> DwiTheetMastery <$> runMessage msg attrs
