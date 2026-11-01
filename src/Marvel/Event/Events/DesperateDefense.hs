module Marvel.Event.Events.DesperateDefense (
  desperateDefense,
  DesperateDefense (..),
) where

import Marvel.Prelude

import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Id
import Marvel.Identity.Types (Field (PlayerIdentityDamage))
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Ref
import Marvel.Window qualified as W

desperateDefense :: EventCard DesperateDefense
desperateDefense =
  event (DesperateDefense . (`With` Meta Nothing)) Cards.desperateDefense

-- | The hero defending, for that attack.
newtype Meta = Meta {defending :: Maybe IdentityId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype DesperateDefense = DesperateDefense (Attrs Event `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsEvent DesperateDefense where
  toEventAttrs (DesperateDefense (attrs `With` _)) = attrs

instance IsRef DesperateDefense where
  toRef = toRef . toEventAttrs

-- | +2 DEF for that attack.
instance HasModifiersFor DesperateDefense where
  getModifiersFor _ (IdentityRef ident) (DesperateDefense (_ `With` meta))
    | defending meta == Just ident = pure [DefenseModifier 2]
  getModifiersFor _ _ _ = pure []

-- | When your hero defends: +2 DEF for that attack, and if you take no damage from it,
-- ready your hero.
instance RunMessage DesperateDefense where
  runMessage msg (DesperateDefense (attrs `With` meta)) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ (Just (W.HeroDefends _ _))) | ident == eventId attrs ->
      pure . DesperateDefense $ attrs `With` Meta (Just identityId)
    IdentityMessage ident (AttackDefended _ damageBefore _) | defending meta == Just ident -> do
      -- a hero defeated by the attack is gone
      alive <- elem ident <$> getAllPlayers
      when alive do
        damage <- project PlayerIdentityDamage ident
        when (damage <= damageBefore) $ ready ident
      pure . DesperateDefense $ attrs `With` Meta Nothing
    _ -> DesperateDefense . (`With` meta) <$> runMessage msg attrs
