module Marvel.Event.Events.ForcefieldProjection where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Ref
import Marvel.Resource
import Marvel.Window qualified as W

forcefieldProjection :: EventCard ForcefieldProjection
forcefieldProjection = event ForcefieldProjection Cards.forcefieldProjection

newtype ForcefieldProjection = ForcefieldProjection (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage ForcefieldProjection where
  runMessage msg e@(ForcefieldProjection attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment (Just window)) | ident == eventId attrs -> do
      case window of
        W.IdentityTakeDamage hero _ -> preventDamage hero 3
        W.AllyTakeDamage ally _ -> preventAllyDamage ally 3
        _ -> pure ()
      wild <- paidWith Wild payment
      when wild $ do
        enemies <- selectList DamageableEnemy
        unless (null enemies) $ chooseOne you [targeting (enemyRef enemy) (dealDamage enemy attrs $ toDamage 3 FromAbility) | enemy <- enemies]
      pure e
    _ -> ForcefieldProjection <$> runMessage msg attrs
