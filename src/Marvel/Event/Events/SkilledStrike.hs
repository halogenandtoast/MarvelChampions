module Marvel.Event.Events.SkilledStrike (
  skilledStrike,
  SkilledStrike (..),
) where

import Marvel.Prelude

import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Message
import Marvel.Modifier
import Marvel.Ref
import Marvel.Window qualified as W

skilledStrike :: EventCard SkilledStrike
skilledStrike = event (SkilledStrike . (`With` Meta Nothing)) Cards.skilledStrike

-- | The hero making the basic attack, for that attack.
newtype Meta = Meta {attacking :: Maybe IdentityId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype SkilledStrike = SkilledStrike (Attrs Event `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsEvent SkilledStrike where
  toEventAttrs (SkilledStrike (attrs `With` _)) = attrs

instance IsRef SkilledStrike where
  toRef = toRef . toEventAttrs

-- | +2 ATK for that attack.
instance HasModifiersFor SkilledStrike where
  getModifiersFor _ (IdentityRef ident) (SkilledStrike (_ `With` meta))
    | attacking meta == Just ident = pure [AttackModifier 2]
  getModifiersFor _ _ _ = pure []

-- | The boost lasts until the basic attack's damage is counted: the identity counts it
-- on the same message, before events see it.
instance RunMessage SkilledStrike where
  runMessage msg (SkilledStrike (attrs `With` meta)) = case msg of
    EventMessage ident (PlayedEvent identityId _ (Just (W.MadeBasicAttack _))) | ident == eventId attrs ->
      pure . SkilledStrike $ attrs `With` Meta (Just identityId)
    IdentityMessage ident (SideMessage BasicAttack) | attacking meta == Just ident ->
      pure . SkilledStrike $ attrs `With` Meta Nothing
    _ -> SkilledStrike . (`With` meta) <$> runMessage msg attrs
