module Marvel.Event.Events.Nc48018 (nc48018, Nc48018 (..)) where

import Marvel.Prelude
import Marvel.Attack
import Marvel.Card ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source ()
import Marvel.Id
import Marvel.Identity.Types (Field (PlayerIdentityAttackDamageTaken))
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Ref
import Marvel.Window qualified as W

nc48018 :: EventCard Nc48018
nc48018 = event (Nc48018 . (`With` Meta Nothing)) Cards.nc48018

newtype Meta = Meta (Maybe (IdentityId, EnemyId))
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
newtype Nc48018 = Nc48018 (Attrs Event `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)
instance IsEvent Nc48018 where
  toEventAttrs (Nc48018 (attrs `With` _)) = attrs
instance IsRef Nc48018 where
  toRef = toRef . toEventAttrs
instance HasModifiersFor Nc48018
instance RunMessage Nc48018 where
  runMessage msg (Nc48018 (attrs `With` meta@(Meta defending))) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just (W.HeroDefends _ enemy))) | eid == eventId attrs -> do
      addToIdentityBasicPower you 2
      pure $ Nc48018 (attrs `With` Meta (Just (you,enemy)))
    IdentityMessage you (IdentityWasAttacked attack) | Just (ident, enemy) <- defending, ident == you, attackEnemy attack == enemy -> do
      before <- project PlayerIdentityAttackDamageTaken you
      registerPostAttackDamage you enemy before attrs 3
      pure $ Nc48018 (attrs `With` Meta Nothing)
    _ -> Nc48018 . (`With` meta) <$> runMessage msg attrs
