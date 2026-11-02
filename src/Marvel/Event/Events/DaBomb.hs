module Marvel.Event.Events.DaBomb where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Window qualified as W ()

daBomb :: EventCard DaBomb
daBomb = event DaBomb Cards.daBomb

newtype DaBomb = DaBomb (Attrs Event)
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage DaBomb where
  runMessage msg e@(DaBomb attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      villains <- selectList VillainEnemy
      chooseOne you [targeting (enemyRef villain) do
        dealDamage villain attrs (toDamage 10 FromAbility)
        cardSpecific attrs "everyone" ()
        | villain <- villains]
      pure e
    CardSpecific (isTarget attrs -> True) "everyone" _ -> do
      amount <- getEncounterIconCount
      enemies <- selectList AnyEnemy
      heroes <- selectList HeroIdentity
      let enemyCharacters = map (\case EnemyVillainId vid -> VillainCharacter vid; EnemyMinionId mid -> MinionCharacter mid; EnemyAttachmentId aid -> AttachmentCharacter aid) enemies
      damageCharacters attrs (toDamage amount FromAbility) (enemyCharacters <> map IdentityCharacter heroes)
      pure e
    _ -> DaBomb <$> runMessage msg attrs
