module Marvel.Event.Events.IceBlast where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Identity.Types ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers
import Marvel.Event.Types

iceBlast :: EventCard IceBlast
iceBlast = event IceBlast Cards.iceBlast

newtype IceBlast = IceBlast (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage IceBlast where
  runMessage msg a@(IceBlast attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      players <- getPlayers
      chooseOne you [targeting p $ cardSpecific attrs "blast" (you, p) | p <- players]
      pure a
    CardSpecific (isTarget attrs -> True) "blast" (fromJSON -> Success (you :: IdentityId, p :: IdentityId)) -> do
      villains <- selectList ActiveVillain
      minions <- selectList (MinionEngagedWith $ IdentityWithId p)
      cardSpecific attrs "attachMany" (you, map EnemyVillainId villains <> map EnemyMinionId minions)
      pure a
    CardSpecific (isTarget attrs -> True) "attachMany" (fromJSON -> Success (you :: IdentityId, enemies :: [EnemyId])) -> do
      case enemies of
        [] -> cardSpecific attrs "damageAll" you
        enemy:rest -> do
          attachFrostbite you enemy
          cardSpecific attrs "attachMany" (you, rest)
      pure a
    CardSpecific (isTarget attrs -> True) "damageAll" (fromJSON -> Success (_you :: IdentityId)) -> do
      enemies <- selectList (DamageableEnemy <> frostbitten)
      dealDamageToEach enemies attrs (toDamage 3 FromAbility)
      pure a
    _ -> IceBlast <$> runMessage msg attrs
