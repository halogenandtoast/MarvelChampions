module Marvel.Upgrade.Upgrades.NavigationColumn (
  navigationColumn,
  NavigationColumn (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

navigationColumn :: UpgradeCard NavigationColumn
navigationColumn = upgrade NavigationColumn Cards.navigationColumn

newtype NavigationColumn = NavigationColumn (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: Exhaust Navigation Column, choose and discard 1 card from your hand
-- (discard the top card of your deck instead if you control the Milano) → draw 1 card.
-- The discard is the cost, decided as it is paid: without the Milano it needs a card in
-- hand.
instance HasAbilities NavigationColumn where
  getAbilities a =
    [ ability
        a
        1
        HeroAction
        OwnsThis
        (ExhaustCost <> IfControlsMilanoCost DiscardTopOfDeckCost (DiscardHandCardsCost 1 (Just 1)))
        (runAbility a 1)
    ]

instance RunMessage NavigationColumn where
  runMessage msg u@(NavigationColumn attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      drawCards you 1
      pure u
    _ -> NavigationColumn <$> runMessage msg attrs
