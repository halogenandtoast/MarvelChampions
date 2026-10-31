module Marvel.Event.Events.FastballSpecial where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Keyword
import Marvel.Window qualified as W
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Count

fastballSpecial :: EventCard FastballSpecial
fastballSpecial = event FastballSpecial Cards.fastballSpecial

newtype FastballSpecial = FastballSpecial (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage FastballSpecial where
  runMessage msg e@(FastballSpecial attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      heroes <- selectList (IdentityWithTitle "Colossus")
      wolverines <- selectList (IdentityWithTitle "Wolverine")
      colossusAllies <- selectList (AllyNamedAs "Colossus")
      wolverineAllies <- selectList (AllyNamedAs "Wolverine")
      let allies = colossusAllies <> wolverineAllies
      heroAttack <- sum <$> traverse (selectCount HeroAttackDamage . IdentityWithId) (heroes <> wolverines)
      allyAttack <- sum <$> traverse (selectCount AllyAttackValue . AllyWithId) allies
      chooseDamage you attrs (withPiercing $ withOverkill $ toDamage (heroAttack + allyAttack) $ FromPlayerAttack you) AttackableEnemy
      pure e
    _ -> FastballSpecial <$> runMessage msg attrs
