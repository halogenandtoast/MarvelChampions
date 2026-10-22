module Marvel.Ally.Allies.MoonKnightMarcSpector (
  moonKnightMarcSpector,
  MoonKnightMarcSpector (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Resource
import Marvel.Window

moonKnightMarcSpector :: AllyCard MoonKnightMarcSpector
moonKnightMarcSpector = ally MoonKnightMarcSpector Cards.moonKnightMarcSpector (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype MoonKnightMarcSpector = MoonKnightMarcSpector (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | A Captive ally: "Response: After you play Moon Knight from your hand, spend a [Wild]
-- resource → draw 2 cards."
instance HasAbilities MoonKnightMarcSpector where
  getAbilities a =
    [ limitedWindowAbility a 1 (PlayThis After) Response OwnsThis (ResourceCost $ Just Wild) $
        runAbility a 1
    ]

instance RunMessage MoonKnightMarcSpector where
  runMessage msg a@(MoonKnightMarcSpector attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      drawCards ident 2
      pure a
    _ -> MoonKnightMarcSpector <$> runMessage msg attrs
