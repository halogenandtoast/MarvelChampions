module Marvel.Support.Supports.TheTriskellion where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

theTriskellion :: SupportCard TheTriskellion
theTriskellion = support TheTriskellion Cards.theTriskellion

instance HasAbilities TheTriskellion where
  getAbilities _ = []

instance HasModifiersFor TheTriskellion where
  getModifiersFor _ (IdentityRef iid) (TheTriskellion a)
    | iid == supportController a =
        pure [AllyLimitModifier 1]
  getModifiersFor _ _ _ = pure []

newtype TheTriskellion = TheTriskellion (Attrs Support)
  deriving anyclass (IsSupport)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage TheTriskellion where
  runMessage msg (TheTriskellion a) = TheTriskellion <$> runMessage msg a
