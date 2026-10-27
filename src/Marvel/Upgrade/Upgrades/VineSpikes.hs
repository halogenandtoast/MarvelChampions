module Marvel.Upgrade.Upgrades.VineSpikes (
  vineSpikes,
  VineSpikes (..),
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

vineSpikes :: UpgradeCard VineSpikes
vineSpikes = upgrade VineSpikes Cards.vineSpikes

newtype VineSpikes = VineSpikes (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Interrupt: When Groot makes a basic attack, remove 1 growth counter from him and
-- exhaust Vine Spikes → Groot gets +2 ATK for that attack.
instance HasAbilities VineSpikes where
  getAbilities a =
    [ limitedWindowAbility
        a
        1
        (MakesBasicAttack When You)
        HeroInterrupt
        (OwnsThis <> InHeroForm)
        (IdentityCountersCost 1 <> ExhaustCost)
        (runAbility a 1)
    ]

instance RunMessage VineSpikes where
  runMessage msg u@(VineSpikes attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      addToBasicPower ident 2
      pure u
    _ -> VineSpikes <$> runMessage msg attrs
