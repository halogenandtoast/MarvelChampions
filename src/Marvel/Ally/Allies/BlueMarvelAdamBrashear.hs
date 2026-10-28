module Marvel.Ally.Allies.BlueMarvelAdamBrashear (
  blueMarvelAdamBrashear,
  BlueMarvelAdamBrashear (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Window

blueMarvelAdamBrashear :: AllyCard BlueMarvelAdamBrashear
blueMarvelAdamBrashear =
  ally BlueMarvelAdamBrashear Cards.blueMarvelAdamBrashear (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype BlueMarvelAdamBrashear = BlueMarvelAdamBrashear (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Hero Response: After Blue Marvel enters play, change energy forms.
instance HasAbilities BlueMarvelAdamBrashear where
  getAbilities a =
    [limitedWindowAbility a 1 EntersPlayThis HeroResponse OwnsThis NoCost $ runAbility a 1]

instance RunMessage BlueMarvelAdamBrashear where
  runMessage msg a@(BlueMarvelAdamBrashear attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      changeEnergyForm ident
      pure a
    _ -> BlueMarvelAdamBrashear <$> runMessage msg attrs
