module Marvel.Minion.Minions.Titania
  ( titania
  , Titania(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

titania :: MinionCard Titania
titania = minion Titania Cards.titania (Sch 1) (Atk 0) (HP 6)

newtype Titania = Titania (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance HasModifiersFor Titania where
  getModifiersFor _ target (Titania attrs) | isTarget attrs target =
    pure [AttackModifier $ minionRemainingHitPoints attrs]
  getModifiersFor _ _ _ = pure []

instance RunMessage Titania where
  runMessage msg (Titania attrs) = Titania <$> runMessage msg attrs
