module Marvel.Minion.Minions.Titania
  ( titania
  , Titania(..)
  ) where

import Marvel.Prelude

import Marvel.Minion.Attrs
import Marvel.Minion.Cards qualified as Cards

titania :: MinionCard Titania
titania = minion Titania Cards.titania (Sch 1) (Atk 0) (HP 6)

newtype Titania = Titania MinionAttrs
  deriving anyclass IsMinion
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasModifiersFor Titania where
  getModifiersFor _ target (Titania attrs) | isTarget attrs target =
    pure [AttackModifier $ minionRemainingHitPoints attrs]
  getModifiersFor _ _ _ = pure []

instance RunMessage Titania where
  runMessage msg (Titania attrs) = Titania <$> runMessage msg attrs
