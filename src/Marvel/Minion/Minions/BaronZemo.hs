module Marvel.Minion.Minions.BaronZemo (
  baronZemo,
  BaronZemo (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

baronZemo :: MinionCard BaronZemo
baronZemo = minion BaronZemo Cards.baronZemo (Sch 1) (Atk 3) (HP 5)

newtype BaronZemo = BaronZemo (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | The player engaged with him cannot thwart.
instance HasModifiersFor BaronZemo where
  getModifiersFor _ (IdentityRef ident) (BaronZemo a)
    | ident == minionEngagedIdentity a = pure [CannotThwart]
  getModifiersFor _ _ _ = pure []

instance RunMessage BaronZemo where
  runMessage msg (BaronZemo attrs) = BaronZemo <$> runMessage msg attrs
