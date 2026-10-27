module Marvel.Ally.Allies.LadySif (
  ladySif,
  LadySif (..),
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

ladySif :: AllyCard LadySif
ladySif = ally LadySif Cards.ladySif (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype LadySif = LadySif (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities LadySif where
  getAbilities a =
    [limitedWindowAbility a 1 EntersPlayThis Response OwnsThis NoCost $ runAbility a 1]

-- | "Ready Thor or Odinson": the player whose identity it is, whoever controls her.
instance RunMessage LadySif where
  runMessage msg a@(LadySif attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      thor <- (<>) <$> selectList (IdentityWithTitle "Thor") <*> selectList (IdentityWithTitle "Odinson")
      for_ thor ready
      pure a
    _ -> LadySif <$> runMessage msg attrs
