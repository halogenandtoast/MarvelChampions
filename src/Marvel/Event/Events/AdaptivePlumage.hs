module Marvel.Event.Events.AdaptivePlumage where

import Marvel.Prelude
import Marvel.Ability qualified as Ability
import Marvel.Card
import Marvel.Card.Side ()
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Window qualified as W ()

adaptivePlumage :: EventCard AdaptivePlumage
adaptivePlumage = event AdaptivePlumage Cards.adaptivePlumage

newtype AdaptivePlumage = AdaptivePlumage (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent AdaptivePlumage where
  eventAbilitySubTypes (AdaptivePlumage attrs) = do
    angel <- identityMatches (IdentityWithTitle "Angel") (eventController attrs)
    pure [if angel then Ability.Thwart else Ability.Attack]

instance RunMessage AdaptivePlumage where
  runMessage msg e@(AdaptivePlumage attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      angel <- identityMatches (IdentityWithTitle "Angel") you
      if angel then do
        schemes <- filterM (canRemoveThreatFrom $ toSource attrs) =<< selectList SchemeYouCanThwart
        enemies <- selectList EnemyCanBeConfused
        let withEnemy scheme = case enemies of
              [] -> thwartSchemeAs you scheme attrs 3
              _ -> chooseOne you [targeting (enemyRef enemy) $
                thwartSchemeAnd you scheme attrs 3 (confuse enemy attrs) | enemy <- enemies]
        if null schemes then
          chooseOne you [targeting (enemyRef enemy) (confuse enemy attrs) | enemy <- enemies]
        else chooseOne you [targeting (schemeRef scheme) (withEnemy scheme) | scheme <- schemes]
      else do
        enemies <- selectList AttackableEnemy
        chooseOne you [targeting (enemyRef enemy) $ dealDamageAnd enemy attrs (toDamage 4 $ FromPlayerAttack you) (stun enemy attrs) | enemy <- enemies]
      pure e
    CardSpecific (isTarget attrs -> True) "confuse" (fromJSON -> Success you) -> do
      enemies <- selectList AnyEnemy
      chooseOne you [targeting (enemyRef enemy) (confuse enemy attrs) | enemy <- enemies]
      pure e
    _ -> AdaptivePlumage <$> runMessage msg attrs
