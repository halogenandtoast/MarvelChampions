module Marvel.Event.Events.JubileePack where

import Marvel.Prelude
import Marvel.Name qualified as Name
import Data.List (nub)
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Types
import Marvel.Id
import Marvel.Jubilee.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Ref
import Marvel.Resource.Types
import Marvel.Window qualified as W

newtype JubileeEvent = JubileeEvent (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

jubilee47006, jubilee47007a, jubilee47007b, jubilee47007c, jubilee47008a, jubilee47008b, jubilee47008c, jubilee47009, jubilee47014, jubilee47015, jubilee47021, jubilee47022, jubilee47028, jubilee47029 :: EventCard JubileeEvent
jubilee47006 = event JubileeEvent Cards.jubilee47006
jubilee47007a = event JubileeEvent Cards.jubilee47007a
jubilee47007b = event JubileeEvent Cards.jubilee47007b
jubilee47007c = event JubileeEvent Cards.jubilee47007c
jubilee47008a = event JubileeEvent Cards.jubilee47008a
jubilee47008b = event JubileeEvent Cards.jubilee47008b
jubilee47008c = event JubileeEvent Cards.jubilee47008c
jubilee47009 = event JubileeEvent Cards.jubilee47009
jubilee47014 = event JubileeEvent Cards.jubilee47014
jubilee47015 = event JubileeEvent Cards.jubilee47015
jubilee47021 = event JubileeEvent Cards.jubilee47021
jubilee47022 = event JubileeEvent Cards.jubilee47022
jubilee47028 = event JubileeEvent Cards.jubilee47028
jubilee47029 = event JubileeEvent Cards.jubilee47029

instance RunMessage JubileeEvent where
  runMessage msg e@(JubileeEvent attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you payment window) | eid == eventId attrs -> do
      resources <- nub <$> paymentResources payment
      let count = length resources
      case Name.title (cdName (eventCardDef attrs)) of
        "Blinding Flash" -> cardSpecific attrs "blindingFlash" (count, [] :: [EnemyId])
        "Firecracker" -> do
          enemies <- selectList AttackableEnemy
          chooseOne you [targeting (enemyRef enemy) do
            dealDamage enemy attrs (toDamage 4 $ FromPlayerAttack you)
            when (count == 2) $ stun enemy attrs
            | enemy <- enemies]
        "Flash of Light" -> do
          removeThreat you attrs 3 ThwartableScheme
          when (count == 2) $ cardSpecific attrs "confuse" ()
        "Grande Finale" -> cardSpecific attrs "grandeFinale" (count + 1)
        "Waylay" -> do
          let cleared = case window of
                Just (W.ThwartRemovedThreat _ _ before removed) -> before > 0 && removed >= before
                Just (W.ThwartedAllThreat _ _) -> True
                _ -> False
          enemies <- selectList AttackableEnemy
          chooseOne you [targeting (enemyRef enemy) (dealDamage enemy attrs $ toDamage (if cleared then 7 else 4) $ FromPlayerAttack you) | enemy <- enemies]
        "Three Steps Ahead" -> cardSpecific attrs "threeStepsAhead" count
        "Multitalented" -> do
          when (Physical `elem` resources) $ cardSpecific attrs "multitalentedDamage" ()
          when (Mental `elem` resources) $ removeThreat you attrs 2 ThwartableScheme
          when (Energy `elem` resources) $ heal you 2
        "Unlikely Duo" -> do
          enemies <- selectList EnemyCanBeConfused
          chooseOne you [targeting (enemyRef enemy) $ confuse enemy attrs | enemy <- enemies]
          cardSpecific attrs "unlikelyDuoDamage" ()
        "Mutant Mayhem" -> jubileeMutantMayhem you attrs payment
        "Serve and Protect" -> jubileeServeAndProtect attrs payment window
        _ -> pure ()
      pure e
    CardSpecific (isTarget attrs -> True) "confuse" _ -> do
      enemies <- selectList EnemyCanBeConfused
      chooseOne (eventController attrs) [targeting (enemyRef enemy) $ confuse enemy attrs | enemy <- enemies]
      pure e
    CardSpecific (isTarget attrs -> True) "blindingFlash" (fromJSON -> Success (remaining :: Int, chosen :: [EnemyId])) -> do
      enemies <- filterM (\enemy -> (&&) (enemy `notElem` chosen) <$> ((||) <$> enemyMatches EnemyCanBeStunned enemy <*> enemyMatches EnemyCanBeConfused enemy)) =<< selectList AnyEnemy
      if remaining <= 0 || null enemies
        then for_ chosen $ \enemy -> stun enemy attrs >> confuse enemy attrs
        else chooseOne (eventController attrs) [targeting (enemyRef enemy) $ cardSpecific attrs "blindingFlash" (remaining - 1, enemy : chosen) | enemy <- enemies]
      pure e
    CardSpecific (isTarget attrs -> True) "grandeFinale" (fromJSON -> Success (remaining :: Int)) -> do
      when (remaining > 0) do
        enemies <- selectList AttackableEnemy
        chooseOne (eventController attrs) [targeting (enemyRef enemy) do
          dealDamage enemy attrs (toDamage 2 $ FromPlayerAttack $ eventController attrs)
          cardSpecific attrs "grandeFinale" (remaining - 1)
          | enemy <- enemies]
      pure e
    CardSpecific (isTarget attrs -> True) "threeStepsAhead" (fromJSON -> Success (remaining :: Int)) -> do
      when (remaining > 0) do
        removeThreat (eventController attrs) attrs 2 ThwartableScheme
        cardSpecific attrs "threeStepsAhead" (remaining - 1)
      pure e
    CardSpecific (isTarget attrs -> True) "multitalentedDamage" _ -> do
      enemies <- selectList AttackableEnemy
      chooseOne (eventController attrs) [targeting (enemyRef enemy) $ dealDamage enemy attrs $ toDamage 2 $ FromPlayerAttack $ eventController attrs | enemy <- enemies]
      pure e
    CardSpecific (isTarget attrs -> True) "unlikelyDuoDamage" _ -> do
      enemies <- selectList (AttackableEnemy <> ConfusedEnemy)
      chooseOne (eventController attrs) [targeting (enemyRef enemy) $ dealDamage enemy attrs $ toDamage 4 $ FromPlayerAttack $ eventController attrs | enemy <- enemies]
      pure e
    _ -> JubileeEvent <$> runMessage msg attrs

-- The returned cards retain their owners even when another player controlled them.
jubileeMutantMayhem :: ReverseQueue m => IdentityId -> Attrs Event -> Payment -> m ()
jubileeMutantMayhem you _ payment =
  for_ [card | ReturnedAllyPayment card <- payments payment] $ \card ->
    playCardIgnoringResourceCost (fromMaybe you $ pcOwner card) card

jubileeServeAndProtect :: ReverseQueue m => Attrs Event -> Payment -> Maybe W.WindowType -> m ()
jubileeServeAndProtect _ payment window = case window of
  Just (W.ThreatPlaced _ scheme _) -> do
    cancelMatchingMessage (isThreatPlacement scheme)
    for_ (paymentExhaustedCharacters payment) $ \character -> giveStatus character ToughStatus
  _ -> pure ()
