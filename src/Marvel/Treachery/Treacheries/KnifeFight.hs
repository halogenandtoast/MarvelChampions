module Marvel.Treachery.Treacheries.KnifeFight where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Status ()
import Marvel.Window qualified as W ()
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

knifeFight :: TreacheryCard KnifeFight
knifeFight = treachery KnifeFight Cards.knifeFight

newtype KnifeFight = KnifeFight (Attrs Treachery)
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage KnifeFight where
  runMessage msg t@(KnifeFight attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      hero <- identityMatches HeroIdentity you
      if not hero then gainSurge attrs
      else do
        enemies <- selectList EnemyWithHighestAttack
        unless (null enemies) $ chooseOne you
          [ targeting (enemyRef enemy) do
              atk <- characterAttackValue (toCharacter enemy)
              heroAtk <- characterAttackValue (IdentityCharacter you)
              damageIdentity you attrs (toDamage atk FromTreachery)
              cardSpecific attrs "strikeBack" (you, enemy, heroAtk)
          | enemy <- enemies
          ]
      pure t
    CardSpecific (isTarget attrs -> True) "strikeBack" (fromJSON -> Success (you, enemy :: EnemyId, beforeDefeat :: Natural)) -> do
      survives <- selectAny (IdentityWithId you)
      atk <- if survives then characterAttackValue (IdentityCharacter you) else pure beforeDefeat
      dealDamage enemy attrs (toDamage atk FromTreachery)
      pure t
    _ -> KnifeFight <$> runMessage msg attrs
