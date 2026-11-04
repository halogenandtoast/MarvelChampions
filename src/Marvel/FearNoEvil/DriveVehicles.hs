module Marvel.FearNoEvil.DriveVehicles where

import Marvel.Prelude
import Data.List (sortOn)
import Marvel.Ability
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Entity
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Treachery.Types
import Marvel.Trait qualified as T
import Marvel.Window qualified as W

newtype Vehicle = Vehicle (Attrs Attachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass IsAttachment
vehicles :: [AttachmentCard Vehicle]
vehicles = map (attachment Vehicle) [Cards.street60186,Cards.street60187,Cards.street60188]
vehicleHost :: Attrs Attachment -> Maybe CharacterId
vehicleHost a = (toCharacter <$> attachmentEnemy a)
  <|> (IdentityCharacter <$> attachmentIdentity a) <|> (AllyCharacter <$> attachmentAlly a)
instance HasModifiersFor Vehicle where
  getModifiersFor _ target (Vehicle a) = pure $ if Just target == (characterRef <$> vehicleHost a) then case toCardCode a of
    "60186" -> [AttackModifier 1]
    "60187" -> [case target of IdentityRef _ -> ThwartModifier 1; AllyRef _ -> ThwartModifier 1; _ -> SchemeModifier 1]
    _ -> []
    else []
instance HasAbilities Vehicle where
  getAbilities (Vehicle a) = [windowAbility a 1 (window host) ForcedInterrupt NoCost (runAbility a 1) | Just host <- [vehicleHost a]]
   where
    window host = case attachmentEnemy a of
      Just enemy -> W.EnemyWouldTakeDamage $ EnemyWithId enemy
      Nothing -> W.CharacterWouldTakeDamage $ CharacterWithId host
instance RunMessage Vehicle where
  runMessage msg e@(Vehicle a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      eligible <- selectList (NotEnemy $ EnemyWithAttachment $ AttachmentWithTrait T.Vehicle)
        >>= filterM (\enemy -> all (`notElem` [CannotHaveAttachedCards,CannotHaveAttachedEncounterCards]) <$> getModifiers (enemyRef enemy))
      values <- for eligible $ \enemy -> (enemy,) <$> case toCardCode a of
        "60186" -> getEnemyAttackValue enemy
        "60187" -> getEnemyScheme enemy
        _ -> getEnemyRemainingHitPoints enemy
      let sorted = sortOn (\(_,n) -> if toCardCode a == "60188" then toInteger n else negate $ toInteger n) values
          candidates = case headMay sorted of Just (_,best) -> [enemy | (enemy,n) <- values,n == best]; Nothing -> []
      if null candidates then removeFromPlay a >> surge you else chooseOne you [targeting (enemyRef enemy) (attachTo aid enemy) | enemy <- candidates]
      pure e
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      for_ (vehicleHost a) $ \host -> for_ (headMay $ mapMaybe (damageFor host) windows) $
        placeCharacterDamageOnAttachment host (attachmentId a)
      pure e
    AttachmentMessage aid (AttachmentDamaged n) | aid == attachmentId a -> do
      let total = attachmentDamage a + n
          limit = case toCardCode a of "60186" -> 4; "60187" -> 3; _ -> 6
      when (total >= limit) $ removeFromPlay a
      pure $ Vehicle a {attachmentDamage=total}
    _ -> Vehicle <$> runMessage msg a
   where
    damageFor host = \case
      W.EnemyWouldBeDamaged enemy _ d | toCharacter enemy == host -> Just d
      W.IdentityTakeDamage you d | IdentityCharacter you == host -> Just d
      W.AllyTakeDamage aid d | AllyCharacter aid == host -> Just d
      _ -> Nothing

newtype Carjacking = Carjacking (Attrs Treachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass IsTreachery
carjacking :: TreacheryCard Carjacking
carjacking = treachery Carjacking Cards.street60190
instance RunMessage Carjacking where
  runMessage msg e@(Carjacking a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      discardUntil FromEncounterDeck (CardWithType AttachmentType <> CardWithTrait T.Vehicle) a
      pure $ Carjacking a {treacheryResolver=Just you}
    WithDiscardedMatch (isTarget a -> True) FromEncounterDeck (EncounterCard card) -> do
      you <- maybe getYou pure (treacheryResolver a)
      attachments <- selectList (AttachmentWithTrait T.Vehicle)
      hasVehicle <- anyM (fmap (== Just (IdentityCharacter you)) . getAttachedCharacter) attachments
      modifiers <- getModifiers (IdentityRef you)
      let cost = SameResourcesCost 3
          attachable = all (`notElem` modifiers) [CannotHaveAttachedCards,CannotHaveAttachedEncounterCards]
      payable <- canAffordCost you (toSource a) cost
      chooseOne you $ [labeled "$label.fneTakeVehicle" (payCostThenWithPayment you a cost a "carjacked" (you,card)) | not hasVehicle && attachable && payable]
        <> [labeled "$label.fneRevealVehicle" (revealFoundEncounterCard you card)]
      pure e
    CardSpecific (isTarget a -> True) "carjacked" (fromJSON -> Success ((you,card),_ :: Payment)) -> do
      attachEncounterToCharacter card (IdentityCharacter you)
      pure e
    _ -> Carjacking <$> runMessage msg a
