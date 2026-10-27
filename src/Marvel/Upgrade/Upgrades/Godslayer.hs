module Marvel.Upgrade.Upgrades.Godslayer (
  godslayer,
  Godslayer (..),
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

godslayer :: UpgradeCard Godslayer
godslayer = upgrade Godslayer Cards.godslayer

newtype Godslayer = Godslayer (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Hero Interrupt: When your hero makes a basic attack against a unique enemy,
-- exhaust Godslayer → your hero gets +2 ATK for that attack." Villains are unique.
instance HasAbilities Godslayer where
  getAbilities a =
    [ limitedWindowAbility a 1 (MakesBasicAttackAgainst When You UniqueEnemy) HeroInterrupt OwnsThis ExhaustCost $
        runAbility a 1
    ]

instance RunMessage Godslayer where
  runMessage msg u@(Godslayer attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      addToBasicPower ident 2
      pure u
    _ -> Godslayer <$> runMessage msg attrs
