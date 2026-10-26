module Marvel.Event.Events.NeverBackDown (
  neverBackDown,
  NeverBackDown (..),
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

neverBackDown :: EventCard NeverBackDown
neverBackDown =
  event (NeverBackDown . (`With` Meta Nothing)) Cards.neverBackDown

-- | The hero defending, for that attack.
newtype Meta = Meta {defending :: Maybe IdentityId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype NeverBackDown = NeverBackDown (Attrs Event `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsEvent NeverBackDown where
  toEventAttrs (NeverBackDown (attrs `With` _)) = attrs

instance IsRef NeverBackDown where
  toRef = toRef . toEventAttrs

-- | +2 DEF for that attack.
instance HasModifiersFor NeverBackDown where
  getModifiersFor _ (IdentityRef ident) (NeverBackDown (_ `With` meta))
    | defending meta == Just ident = pure [DefenseModifier 2]
  getModifiersFor _ _ _ = pure []

-- | When you defend: +2 DEF for that attack, and if you take no damage from it (a tough
-- status card taking it all counts), stun the attacking enemy.
instance RunMessage NeverBackDown where
  runMessage msg (NeverBackDown (attrs `With` meta)) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ (Just (W.HeroDefends _ _))) | ident == eventId attrs ->
      pure . NeverBackDown $ attrs `With` Meta (Just identityId)
    IdentityMessage ident (AttackDefended enemy damageBefore) | defending meta == Just ident -> do
      -- a hero defeated by the attack is gone
      alive <- elem ident <$> getAllPlayers
      -- nor is an enemy retaliate defeated
      attacker <- selectAny (EnemyWithId enemy)
      when (alive && attacker) do
        damage <- project PlayerIdentityDamage ident
        when (damage <= damageBefore) $ stun enemy attrs
      pure . NeverBackDown $ attrs `With` Meta Nothing
    -- an attack given up before its damage is over all the same
    VillainMessage _ VillainEndAttack -> pure . NeverBackDown $ attrs `With` Meta Nothing
    MinionMessage _ MinionEndAttack -> pure . NeverBackDown $ attrs `With` Meta Nothing
    _ -> NeverBackDown . (`With` meta) <$> runMessage msg attrs
