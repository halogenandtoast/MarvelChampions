module Marvel.FearNoEvil.KingpinEquipment where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability.Types qualified as Ability
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage (toDamage,DamageSource(FromAbility))
import Marvel.Entity
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Trait qualified as T
import Marvel.Window qualified as W

newtype KingpinEquipment = KingpinEquipment (Attrs Attachment)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass IsAttachment
kingpinEquipment :: [AttachmentCard KingpinEquipment]
kingpinEquipment = [attachment (\a -> KingpinEquipment a {attachmentCounters=if toCardCode def == "60164" then 3 else 0}) def | def <- [Cards.street60164,Cards.street60165,Cards.street60166,Cards.street60167,Cards.street60168]]
instance HasModifiersFor KingpinEquipment where
  getModifiersFor _ target (KingpinEquipment a) = pure $ case toCardCode a of
    "60164" | Just target == (IdentityRef <$> attachmentIdentity a) -> [CannotChangeToAlterEgoForm] <> [ReplaceAttackOrThwartWithCounterRemoval $ attachmentId a | attachmentCounters a > 0]
    "60165" | Just target == (enemyRef <$> attachmentEnemy a) -> [SchemeModifier 1]
    "60166" | Just target == (enemyRef <$> attachmentEnemy a) -> [AttackModifier 1]
    "60167" | Just target == (enemyRef <$> attachmentEnemy a) -> [KeywordModifier $ Retaliate 1]
    "60168" | Just target == (enemyRef <$> attachmentEnemy a) -> [SchemeModifier 1]
    _ -> []
instance HasAbilities KingpinEquipment where
  getAbilities (KingpinEquipment a) = case toCardCode a of
    "60165" -> [ability a 1 HeroAction NoCriteria (DiscardMatchingHandCardCost $ CardWithType EventType <> CardWithAbilitySubtype Ability.Thwart) $ runAbility a 1]
    "60168" -> [ability a 1 HeroAction NoCriteria (DiscardMatchingHandCardCost $ CardWithType EventType <> CardWithAbilitySubtype Ability.Attack) $ runAbility a 1]
    "60166" -> [windowAbility a 2 (W.EnemyAttackedAndDamaged (EnemyWithId enemy) (IdentityCharacters HeroIdentity)) ForcedResponse NoCost (runAbility a 2) | Just enemy <- [attachmentEnemy a]]
    "60167" -> [windowAbility a 3 (W.EnemyWouldTakeDamage $ EnemyWithId enemy) ForcedInterrupt NoCost (runAbility a 3) | Just enemy <- [attachmentEnemy a]]
    _ -> []
instance RunMessage KingpinEquipment where
  runMessage msg e@(KingpinEquipment a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      kingpin <- villainNamed "Kingpin"
      martial <- maybe (pure False) (villainMatches $ VillainWithTrait T.MartialArtist) kingpin
      case toCardCode a of
        "60164" -> if martial then attachToIdentity aid you else removeFromPlay a >> surge you
        "60167" -> if martial then traverse_ (attachTo aid) kingpin else removeFromPlay a >> surge you
        code -> do
          traverse_ (attachTo aid) kingpin
          case code of
            "60165" -> traverse_ dealBoost kingpin
            "60166" -> stunIdentity you
            "60168" -> confuseIdentity you
            _ -> pure ()
      pure e
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      let host = attachmentEnemy a
      removeFromPlay a
      for_ host $ \case
        EnemyVillainId vid -> if toCardCode a == "60165" then villainSchemesAgainst vid you else villainAttacksCharacter vid you you
        _ -> pure ()
      pure e
    RanAbility _ (isTarget a -> True) 2 windows _ -> do
      for_ [you | W.EnemyAttacksAndDamages _ (IdentityCharacter you) n <- windows,n > 0] $ \you -> do
        stunned <- identityMatches StunnedIdentity you
        stunIdentity you
        cardSpecific a "caneStunnedHero" (you,stunned)
      pure e
    CardSpecific (isTarget a -> True) "caneStunnedHero" (fromJSON -> Success (you,wasStunned :: Bool)) -> do
      now <- identityMatches StunnedIdentity you
      when (not wasStunned && now) $ removeFromPlay a
      pure e
    RanAbility _ (isTarget a -> True) 3 windows _ -> do
      for_ (attachmentEnemy a) $ \enemy -> for_ [damage | W.EnemyWouldBeDamaged target _ damage <- windows,target == enemy] $ placeCharacterDamageOnAttachment (toCharacter enemy) (attachmentId a)
      pure e
    AttachmentMessage aid (AttachmentDamaged n) | aid == attachmentId a -> do
      let total = attachmentDamage a+n
      when (total >= 8) $ removeFromPlay a
      pure $ KingpinEquipment a {attachmentDamage=total}
    RemoveCounters target n | target == toRef a,toCardCode a == "60164" -> do
      when (attachmentCounters a > 0 && n >= attachmentCounters a) $ removeFromPlay a
      KingpinEquipment <$> runMessage msg a
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      case toCardCode a of
        "60165" -> do
          kingpin <- villainNamed "Kingpin"
          for_ kingpin $ \vid -> case toCard a of
            EncounterCard card -> consumeBoostCard card >> attachEncounterToCharacter card (VillainCharacter vid)
            _ -> pure ()
        "60166" -> do
          already <- identityMatches StunnedIdentity you
          stunIdentity you
          when already $ damageIdentity you a $ toDamage 1 FromAbility
        "60168" -> do
          already <- identityMatches ConfusedIdentity you
          confuseIdentity you
          when already $ placeThreatOnEach a 1 EncounterMainScheme
        _ -> pure ()
      pure e
    _ -> KingpinEquipment <$> runMessage msg a
