module Marvel.Ally.Allies.MockingbirdBobbiMorse where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Query
import Marvel.Window

mockingbirdBobbiMorse :: AllyCard MockingbirdBobbiMorse
mockingbirdBobbiMorse = ally
  MockingbirdBobbiMorse
  Cards.mockingbirdBobbiMorse
  (Thw 1, 1)
  (Atk 1, 1)
  (HP 3)

instance HasAbilities MockingbirdBobbiMorse where
  getAbilities a =
    [ limitedWindowAbility a 1 (PlayThis After) Response OwnsThis NoCost
        $ runAbility a 1
    ]

newtype MockingbirdBobbiMorse = MockingbirdBobbiMorse (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsSource, IsTarget, HasController)

instance RunMessage MockingbirdBobbiMorse where
  runMessage msg a@(MockingbirdBobbiMorse attrs) = case msg of
    RanAbility (isTarget a -> True) 1 _ -> do
      enemies <- selectList AnyEnemy
      push $ Ask (controller a) $ ChooseOne $ stunChoice a <$> enemies
      pure a
    _ -> MockingbirdBobbiMorse <$> runMessage msg attrs
