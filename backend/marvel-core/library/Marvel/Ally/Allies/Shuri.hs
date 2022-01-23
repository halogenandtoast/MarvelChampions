module Marvel.Ally.Allies.Shuri (
  shuri,
  Shuri (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Attrs
import Marvel.Ally.Cards qualified as Cards
import Marvel.Cost
import Marvel.Criteria
import Marvel.Window

shuri :: AllyCard Shuri
shuri = ally Shuri Cards.shuri (Thw 1, 1) (Atk 1, 1) (HP 3)

newtype Shuri = Shuri AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasController, Entity, IsSource, IsTarget)

instance HasAbilities Shuri where
  getAbilities a =
    [ limitedWindowAbility a 1 (PlayThis After) Response OwnsThis NoCost $
        runAbility a 1
    ]

instance RunMessage Shuri where
  runMessage msg a = case msg of
    RanAbility (isTarget a -> True) 1 _ -> do
      push . controllerMessage a $
        Search
          (SearchIdentityDeck (controller a) AllOfDeck)
          (CardWithType UpgradeType)
          SearchDrawOne
          ShuffleBackIn
      pure a
    _ -> Shuri <$> runMessage msg (toAttrs a)
