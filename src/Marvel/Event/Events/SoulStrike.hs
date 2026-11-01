module Marvel.Event.Events.SoulStrike where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Card
import Marvel.Damage
import Marvel.Deck
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Projection
import Marvel.Query
import Marvel.Resource.Types

soulStrike :: EventCard SoulStrike
soulStrike = event SoulStrike Cards.soulStrike

newtype SoulStrike = SoulStrike (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage SoulStrike where
  runMessage msg e@(SoulStrike attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      chooseOne you [targeting (enemyRef enemy) do
        dealDamage enemy attrs (toDamage 4 $ FromPlayerAttack you)
        cardSpecific attrs "stun" (you, enemy)
        | enemy <- enemies]
      pure e
    CardSpecific (isTarget attrs -> True) "stun" (fromJSON -> Success (you, enemy :: EnemyId)) -> do
      deck <- projectMap PlayerIdentityDeck unDeck you
      alive <- selectAny (EnemyWithId enemy)
      when (alive && maybe False (any (`elem` [Physical, Wild]) . printedResources . getCardDef) (headMay deck)) $
        stun enemy attrs
      pure e
    _ -> SoulStrike <$> runMessage msg attrs
