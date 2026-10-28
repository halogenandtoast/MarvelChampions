module Marvel.Ally.Allies.QuasarWendellVaughn (
  quasarWendellVaughn,
  QuasarWendellVaughn (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Query
import Marvel.Window

quasarWendellVaughn :: AllyCard QuasarWendellVaughn
quasarWendellVaughn = ally QuasarWendellVaughn Cards.quasarWendellVaughn (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype QuasarWendellVaughn = QuasarWendellVaughn (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Response: After Quasar enters play, remove 1 threat from each scheme in play (each
-- one threat can be removed from, as Hyper Thrusters).
instance HasAbilities QuasarWendellVaughn where
  getAbilities a =
    [limitedWindowAbility a 1 EntersPlayThis Response (OwnsThis <> SchemeExists ThwartableScheme) NoCost $ runAbility a 1]

instance RunMessage QuasarWendellVaughn where
  runMessage msg a@(QuasarWendellVaughn attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      schemes <- selectList ThwartableScheme
      thwartEach attrs [(scheme, 1 :: Natural) | scheme <- schemes]
      pure a
    _ -> QuasarWendellVaughn <$> runMessage msg attrs
