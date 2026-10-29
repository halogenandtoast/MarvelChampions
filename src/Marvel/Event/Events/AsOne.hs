module Marvel.Event.Events.AsOne (asOne, AsOne (..)) where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Count
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Ref
import Marvel.Trait (Trait (Avenger, Guardian))

asOne :: EventCard AsOne
asOne = event AsOne Cards.asOne

newtype AsOne = AsOne (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | The effect must be able to deal damage; paying its exhaustions alone is not
-- a change to the game state. A stun can instead change state by replacing it.
instance IsEvent AsOne where
  eventExhaustionCostValid (AsOne attrs) characters = do
    stunned <- selectAny (IdentityWithId (eventController attrs) <> StunnedIdentity)
    amount <- sum <$> traverse characterAttack characters
    pure $ stunned || amount > 0

  eventTargets (AsOne attrs) = do
    stunned <- selectAny (IdentityWithId (eventController attrs) <> StunnedIdentity)
    avengers <- readyCharacters Avenger
    guardians <- readyCharacters Guardian
    meaningful <- anyM (\(a, g) -> (> 0) <$> ((+) <$> characterAttack a <*> characterAttack g))
      [(a, g) | a <- avengers, g <- guardians, a /= g]
    pure $ if stunned || meaningful then Nothing else Just []

readyCharacters :: (HasGame m) => Trait -> m [CharacterId]
readyCharacters trait = (<>)
  <$> selectMap IdentityCharacter (IdentityWithTrait trait <> UnexhaustedIdentity)
  <*> selectMap AllyCharacter (AllyWithTrait trait <> UnexhaustedAlly)

characterAttack :: (HasGame m) => CharacterId -> m Natural
characterAttack = \case
  IdentityCharacter hero -> selectCount HeroAttackDamage (IdentityWithId hero)
  AllyCharacter ally -> selectCount AllyAttackValue (AllyWithId ally)
  _ -> pure 0

instance RunMessage AsOne where
  runMessage msg e@(AsOne attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment _) | ident == eventId attrs -> do
      attacks <- traverse characterAttack (paymentExhaustedCharacters payment)
      chooseDamage you attrs (withOverkill $ toDamage (sum attacks) $ FromPlayerAttack you) AttackableEnemy
      pure e
    _ -> AsOne <$> runMessage msg attrs
