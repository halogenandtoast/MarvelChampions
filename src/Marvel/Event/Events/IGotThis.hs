module Marvel.Event.Events.IGotThis where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Window qualified as W ()

iGotThis :: EventCard IGotThis
iGotThis = event IGotThis Cards.iGotThis

newtype IGotThis = IGotThis (Attrs Event)
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage IGotThis where
  runMessage msg e@(IGotThis attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _ _ _) | eid == eventId attrs -> do
      (crisis, acceleration, amplify, hazard) <- getEncounterIconCounts
      when (crisis > 0) $ cardSpecific attrs "crisis" ()
      when (acceleration > 0) $ cardSpecific attrs "acceleration" ()
      when (amplify > 0) $ cardSpecific attrs "amplify" ()
      when (hazard > 0) $ cardSpecific attrs "hazard" ()
      pure e
    CardSpecific (isTarget attrs -> True) "crisis" _ -> do
      chooseDamage (eventController attrs) attrs (toDamage 3 FromAbility) AnyEnemy
      pure e
    CardSpecific (isTarget attrs -> True) "acceleration" _ -> do
      removeThreat (eventController attrs) attrs 2 ThwartableScheme
      pure e
    CardSpecific (isTarget attrs -> True) "amplify" _ -> do
      allies <- selectList (AllyControlledBy (IdentityWithId $ eventController attrs) <> AllyCanReady)
      chooseOne (eventController attrs) [targeting aid (ready aid) | aid <- allies]
      pure e
    CardSpecific (isTarget attrs -> True) "hazard" _ -> do
      drawCards (eventController attrs) 1
      pure e
    _ -> IGotThis <$> runMessage msg attrs
