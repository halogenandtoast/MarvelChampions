module Marvel.Ally.Allies.HellcatPatsyWalker
  ( hellcatPatsyWalker
  , HellcatPatsyWalker(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Attrs
import qualified Marvel.Ally.Cards as Cards
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Hp
import Marvel.Message
import Marvel.Question
import Marvel.Source
import Marvel.Stats
import Marvel.Target

hellcatPatsyWalker :: AllyCard HellcatPatsyWalker
hellcatPatsyWalker =
  ally HellcatPatsyWalker Cards.hellcatPatsyWalker (Thw 2, 1) (Atk 1, 1) (HP 3)

newtype HellcatPatsyWalker = HellcatPatsyWalker AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities HellcatPatsyWalker where
  getAbilities (HellcatPatsyWalker a) =
    [ability a 1 Action OwnsThis NoCost $ ReturnTargetToHand $ toTarget a]

instance RunMessage HellcatPatsyWalker where
  runMessage msg (HellcatPatsyWalker attrs) =
    HellcatPatsyWalker <$> runMessage msg attrs
