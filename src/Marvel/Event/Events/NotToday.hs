module Marvel.Event.Events.NotToday (
  notToday,
  NotToday (..),
) where

import Marvel.Prelude

import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Identity.Types (Field (PlayerIdentityDamage))
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.Window qualified as W

notToday :: EventCard NotToday
notToday =
  event (NotToday . (`With` Meta Nothing)) Cards.notToday

-- | The hero defending, for that attack.
newtype Meta = Meta {defending :: Maybe IdentityId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype NotToday = NotToday (Attrs Event `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsEvent NotToday where
  toEventAttrs (NotToday (attrs `With` _)) = attrs

instance IsRef NotToday where
  toRef = toRef . toEventAttrs

-- | +2 DEF for that attack.
instance HasModifiersFor NotToday where
  getModifiersFor _ (IdentityRef ident) (NotToday (_ `With` meta))
    | defending meta == Just ident = pure [DefenseModifier 2]
  getModifiersFor _ _ _ = pure []

-- | When you defend: +2 DEF for that attack, and if you take no damage from it (a tough
-- status card taking it all counts), remove two threat from a scheme.
instance RunMessage NotToday where
  runMessage msg (NotToday (attrs `With` meta)) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ (Just (W.HeroDefends _ _))) | ident == eventId attrs ->
      pure . NotToday $ attrs `With` Meta (Just identityId)
    IdentityMessage ident (AttackDefended enemy damageBefore _) | defending meta == Just ident -> do
      -- a hero defeated by the attack is gone
      alive <- elem ident <$> getAllPlayers
      -- nor is an enemy retaliate defeated
      when alive do
        damage <- project PlayerIdentityDamage ident
        when (damage <= damageBefore) $ removeThreat ident attrs 2 ThwartableScheme
      pure . NotToday $ attrs `With` Meta Nothing
    -- an attack given up before its damage is over all the same
    VillainMessage _ VillainEndAttack -> pure . NotToday $ attrs `With` Meta Nothing
    MinionMessage _ MinionEndAttack -> pure . NotToday $ attrs `With` Meta Nothing
    _ -> NotToday . (`With` meta) <$> runMessage msg attrs
