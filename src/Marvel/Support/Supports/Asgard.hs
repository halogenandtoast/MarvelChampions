module Marvel.Support.Supports.Asgard (
  asgard,
  Asgard (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

asgard :: SupportCard Asgard
asgard = support Asgard Cards.asgard

newtype Asgard = Asgard (Attrs Support)
  deriving anyclass (IsSupport, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Asgard where
  getModifiersFor _ (IdentityRef ident) (Asgard a)
    | ident == supportController a = pure [HandSizeModifier 1]
  getModifiersFor _ _ _ = pure []

instance RunMessage Asgard where
  runMessage msg (Asgard attrs) = Asgard <$> runMessage msg attrs
