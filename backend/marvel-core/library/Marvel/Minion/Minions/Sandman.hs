module Marvel.Minion.Minions.Sandman
  ( sandman
  , Sandman(..)
  ) where

import Marvel.Prelude

import Marvel.Minion.Attrs
import Marvel.Minion.Cards qualified as Cards

sandman :: MinionCard Sandman
sandman =
  minionWith Sandman Cards.sandman (Sch 2) (Atk 3) (HP 4) (toughL .~ True)

newtype Sandman = Sandman MinionAttrs
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage Sandman where
  runMessage msg (Sandman attrs) = Sandman <$> runMessage msg attrs
