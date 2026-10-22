module Marvel.Minion.Minions.TheViper (
  theViper,
  TheViper (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

theViper :: MinionCard TheViper
theViper = minion TheViper Cards.theViper (Sch 2) (Atk 2) (HP 5)

newtype TheViper = TheViper (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | While the Viper is engaged with you, your hand size is reduced by 1.
instance HasModifiersFor TheViper where
  getModifiersFor _ (IdentityRef ident) (TheViper a)
    | ident == minionEngagedIdentity a = pure [HandSizeReduction 1]
  getModifiersFor _ _ _ = pure []

instance RunMessage TheViper where
  runMessage msg (TheViper attrs) = TheViper <$> runMessage msg attrs
