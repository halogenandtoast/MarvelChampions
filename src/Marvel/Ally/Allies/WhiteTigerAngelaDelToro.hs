module Marvel.Ally.Allies.WhiteTigerAngelaDelToro (
  whiteTigerAngelaDelToro,
  WhiteTigerAngelaDelToro (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Resource
import Marvel.Window

whiteTigerAngelaDelToro :: AllyCard WhiteTigerAngelaDelToro
whiteTigerAngelaDelToro = ally WhiteTigerAngelaDelToro Cards.whiteTigerAngelaDelToro (Thw 3, 1) (Atk 1, 1) (HP 3)

newtype WhiteTigerAngelaDelToro = WhiteTigerAngelaDelToro (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | A Captive ally: "Response: After you play White Tiger from your hand, spend a [Mental]
-- resource → remove 3 threat from a scheme."
instance HasAbilities WhiteTigerAngelaDelToro where
  getAbilities a =
    [ limitedWindowAbility a 1 (PlayThis After) Response OwnsThis (ResourceCost $ Just Mental) $
        runAbility a 1
    ]

instance RunMessage WhiteTigerAngelaDelToro where
  runMessage msg a@(WhiteTigerAngelaDelToro attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      removeThreat ident a 3 ThwartableScheme
      pure a
    _ -> WhiteTigerAngelaDelToro <$> runMessage msg attrs
