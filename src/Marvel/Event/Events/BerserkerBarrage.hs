module Marvel.Event.Events.BerserkerBarrage where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Window qualified as W

berserkerBarrage :: EventCard BerserkerBarrage
berserkerBarrage = event BerserkerBarrage Cards.berserkerBarrage

newtype BerserkerBarrage = BerserkerBarrage (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage BerserkerBarrage where
  runMessage msg e@(BerserkerBarrage attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _ _ _) | eid == eventId attrs -> attack
    CardSpecific (isTarget attrs -> True) "attack" _ -> do
      stunned <- identityMatches StunnedIdentity (eventController attrs)
      if stunned then e <$ removeStun (eventController attrs) else attack
    -- Each repetition is another complete attack. Its retaliation and after-attack
    -- responses resolve before the player chooses whether to take damage to repeat.
    EventMessage eid (EventAttacked who enemy ranged) | eid == eventId attrs -> do
      unless ranged $ enemyRetaliates enemy (IdentityCharacter who)
      checkWindows [W.Window W.After $ W.IdentityAttack who enemy]
      checkWindows [W.Window W.After $ W.PlayerAttackResolved (toSource attrs) who [enemy]]
      pure e
    CheckWindows ws -> do
      for_ [enemy | W.Window W.After window <- ws, (enemy, source) <- defeat window, source == toSource attrs] $ \enemy ->
        insteadOf (isAfter enemy) $ cardSpecific attrs "repeat" ()
      for_ [() | W.Window W.After (W.IdentityTookDamage you source damage) <- ws, you == eventController attrs, source == toSource attrs, damageAmount damage >= 2] $ \_ ->
        insteadOf isRepeatDamageDone $ cardSpecific attrs "attack" ()
      BerserkerBarrage <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "repeat" _ -> do
      present <- identityMatches AnyIdentity (eventController attrs)
      when present $ chooseOne (eventController attrs)
        [ labeled "$label.berserkerBarrageRepeat" do
            -- The erratum uses 'to', so this is an effect requiring the full 2 damage
            -- actually taken, rather than a fresh paid ability.
            cardSpecific attrs "beforeRepeatDamage" ()
        , labeled "$label.done" (pure ())
        ]
      pure e
    CardSpecific (isTarget attrs -> True) "beforeRepeatDamage" _ -> do
      damageIdentity (eventController attrs) attrs (toDamage 2 FromAbility)
      cardSpecific attrs "afterRepeatDamage" ()
      pure e
    _ -> BerserkerBarrage <$> runMessage msg attrs
   where
    attack = do
      let you = eventController attrs
      present <- identityMatches AnyIdentity you
      when present do
        enemies <- selectList AttackableEnemy
        chooseOne you [targeting (enemyRef enemy) do
          dealDamage enemy attrs (toDamage 4 $ FromPlayerAttack you)
          cardSpecific attrs "after" enemy
          | enemy <- enemies]
      pure e
    defeat = \case
      W.DefeatedMinion mid _ source _ -> [(EnemyMinionId mid, source)]
      W.DefeatedVillain vid _ source _ -> [(EnemyVillainId vid, source)]
      _ -> []
    isAfter enemy = \case
      CardSpecific (isTarget attrs -> True) "after" (fromJSON -> Success (enemy' :: EnemyId)) -> enemy == enemy'
      _ -> False
    isRepeatDamageDone = \case
      CardSpecific (isTarget attrs -> True) "afterRepeatDamage" _ -> True
      _ -> False
