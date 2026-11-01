module Marvel.Event.Events.MassIncrease where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Ability qualified as Ability ()
import Marvel.Card ()
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Query ()
import Marvel.Id
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question ()
import Marvel.Ref
import Marvel.Trait qualified as Trait ()
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Upgrade.Cards qualified as Upgrades ()
import Marvel.Window qualified as W

massIncrease :: EventCard MassIncrease
massIncrease = event (MassIncrease . (`With` Meta Nothing)) Cards.massIncrease

newtype Meta = Meta {defender :: Maybe (IdentityId, EnemyId)}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype MassIncrease = MassIncrease (Attrs Event `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)
  deriving anyclass (HasModifiersFor)

instance IsEvent MassIncrease where
  toEventAttrs (MassIncrease (attrs `With` _)) = attrs

instance IsRef MassIncrease where
  toRef = toRef . toEventAttrs

instance RunMessage MassIncrease where
  runMessage msg (MassIncrease (attrs `With` meta)) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just (W.HeroDefends _ enemy))) | eid == eventId attrs -> do
      preventAttackDamage enemy
      pure $ MassIncrease (attrs `With` Meta (Just (you, enemy)))
    IdentityMessage you (AttackDefended enemy _ _) | defender meta == Just (you, enemy) -> do
      stun enemy attrs
      pure $ MassIncrease (attrs `With` Meta Nothing)
    _ -> MassIncrease . (`With` meta) <$> runMessage msg attrs
