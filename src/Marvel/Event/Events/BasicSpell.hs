module Marvel.Event.Events.BasicSpell where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Damage
import Marvel.Matchers
import Marvel.Query

basicSpell :: EventCard BasicSpell
basicSpell = event BasicSpell Cards.basicSpell

newtype BasicSpell = BasicSpell (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage BasicSpell where
  runMessage msg e@(BasicSpell attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      hurt <- selectList IdentityWithAnyDamage
      schemes <- selectList ThwartableScheme
      enemies <- selectList DamageableEnemy
      chooseOne you $
        [labeled "$label.magikBasicSpellHeal" $ chooseOne you [targeting who (heal who 3) | who <- hurt] | notNull hurt]
        <> [labeled "$label.magikBasicSpellThwart" (removeThreat you attrs 3 ThwartableScheme) | notNull schemes]
        <> [labeled "$label.magikBasicSpellDamage" (chooseDamage you attrs (toDamage 3 FromAbility) DamageableEnemy) | notNull enemies]
      pure e
    _ -> BasicSpell <$> runMessage msg attrs
