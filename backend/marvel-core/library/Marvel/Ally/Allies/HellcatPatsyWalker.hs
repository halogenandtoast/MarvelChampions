module Marvel.Ally.Allies.HellcatPatsyWalker
  ( hellcatPatsyWalker
  , HellcatPatsyWalker(..)
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

newtype HellcatPatsyWalker = HellcatPatsyWalker AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, IsSource, IsTarget)

instance HasAbilities HellcatPatsyWalker where
  getAbilities a =
    [ability a 1 Action OwnsThis NoCost $ ReturnTargetToHand $ toTarget a]

instance RunMessage HellcatPatsyWalker where
  runMessage msg a = HellcatPatsyWalker <$> runMessage msg (toAttrs a)
