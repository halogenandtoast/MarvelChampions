module Marvel.Upgrade.Upgrades.ArmedToTheTeeth where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

armedToTheTeeth :: UpgradeCard ArmedToTheTeeth
armedToTheTeeth = upgrade ArmedToTheTeeth Cards.armedToTheTeeth

newtype ArmedToTheTeeth = ArmedToTheTeeth (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities ArmedToTheTeeth where
  getAbilities a@(ArmedToTheTeeth attrs) =
    [ limitedWindowAbility a 1 (W.PlayThis W.After) Response OwnsThis NoCost (runAbility a 1)
    , ability a 2 Action (OwnsThis <> HeldWeaponSwapAvailable (upgradeId attrs)) ExhaustCost (runAbility a 2)
    ]

instance RunMessage ArmedToTheTeeth where
  runMessage msg u@(ArmedToTheTeeth attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> u <$ searchCollectionWeapon you (upgradeId attrs)
    RanAbility you (isTarget attrs -> True) 2 _ _ -> do
      swaps <- getLegalHeldWeaponSwaps you (upgradeId attrs)
      unless (null swaps) $ chooseOne you
        [ targeting outgoing (swapHeldWeapon you (upgradeId attrs) outgoing incoming)
        | (outgoing, incoming) <- swaps
        ]
      pure u
    _ -> ArmedToTheTeeth <$> runMessage msg attrs
