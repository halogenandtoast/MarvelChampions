module Marvel.Ally.Allies.HellcatPatsyWalker (
  hellcatPatsyWalker,
  HellcatPatsyWalker (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria

hellcatPatsyWalker :: AllyCard HellcatPatsyWalker
hellcatPatsyWalker =
  ally HellcatPatsyWalker Cards.hellcatPatsyWalker (Thw 2, 1) (Atk 1, 1) (HP 3)

newtype HellcatPatsyWalker = HellcatPatsyWalker (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities HellcatPatsyWalker where
  getAbilities a =
    [ability a 1 Action OwnsThis NoCost $ ReturnTargetToHand $ toRef a]

instance RunMessage HellcatPatsyWalker where
  runMessage msg (HellcatPatsyWalker a) =
    HellcatPatsyWalker <$> runMessage msg a
