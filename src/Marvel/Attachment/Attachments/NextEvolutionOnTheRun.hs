module Marvel.Attachment.Attachments.NextEvolutionOnTheRun where

import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Card
import Marvel.Entity
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Ability
import Data.HashMap.Strict qualified as HashMap ()
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Minion.Types qualified as Minion ()
import Marvel.Name (display)
import Marvel.Query
import Marvel.Trait (Trait (Marauder))
import Marvel.Window qualified as W
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype OnTheRunAttachment = OnTheRunAttachment (Attrs Attachment)
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40105a :: AttachmentCard OnTheRunAttachment
ne40105a = attachment OnTheRunAttachment Cards.ne40105a

ne40105b :: AttachmentCard OnTheRunAttachment
ne40105b = attachment OnTheRunAttachment Cards.ne40105b

ne40106 :: AttachmentCard OnTheRunAttachment
ne40106 = attachment OnTheRunAttachment Cards.ne40106

ne40107 :: AttachmentCard OnTheRunAttachment
ne40107 = attachment OnTheRunAttachment Cards.ne40107

instance HasModifiersFor OnTheRunAttachment where
  getModifiersFor _ target (OnTheRunAttachment a)
    | Just enemy <- attachmentEnemy a, target == enemyRef enemy = case toCardCode a of
      "40105b" -> do
        players <- fromIntegral <$> getStartingPlayerCount
        pure [AttackModifier 1,SchemeModifier 1,HitPointModifier $ 6 * players]
      "40107" -> pure [AttackModifier 1,KeywordModifier Overkill,KeywordModifier Piercing,KeywordModifier Ranged]
      _ -> pure []
  getModifiersFor _ _ _ = pure []

instance HasAbilities OnTheRunAttachment where
  getAbilities (OnTheRunAttachment a) = case attachmentEnemy a of
    Just enemy -> case toCardCode a of
      "40105a" -> captorAttack enemy <> [windowAbility a 2 (W.VillainWouldBeDefeatedMatching $ villainMatcher enemy) ForcedInterrupt NoCost (runAbility a 2)]
      "40105b" -> captorAttack enemy
      "40106" -> [windowAbility a 3 (W.EnemyWouldBeDealtDamage $ EnemyWithId enemy) ForcedInterrupt NoCost (runAbility a 3)]
      "40107" -> [windowAbility a 4 (W.HeroDefendedWithoutDamageAgainst W.After AnyIdentity $ EnemyWithId enemy) HeroResponse NoCost (runAbility a 4)]
      _ -> []
    _ -> []
   where
    villainMatcher = \case EnemyVillainId vid -> VillainWithId vid; _ -> AnyVillain
    captorAttack enemy = [(windowAbility a 1 (W.EnemyWouldAttack (EnemyWithId enemy) AnyIdentity) ForcedInterrupt NoCost (runAbility a 1))
      {abilityCriteria = MinionExists $ MinionWithTrait Marauder <> MinionEngagedWith You}]

instance RunMessage OnTheRunAttachment where
  runMessage msg e@(OnTheRunAttachment a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment _) | aid == attachmentId a -> do
      case toCardCode a of
        "40105a" -> attachToTheVillain a
        "40105b" -> do
          attachToTheVillain a
          cardSpecific a "neCaptorDesperateReveal" ()
        "40106" -> do
          enemies <- selectList AnyEnemy
          remaining <- for enemies \enemy -> (enemy,) <$> enemyRemainingHitPoints enemy
          let lowest = minimum $ map snd remaining
              targets = [enemy | (enemy,hp) <- remaining, hp == lowest]
          first <- selectJust FirstPlayer
          unless (null targets) $ chooseOrRunOne first [targeting (enemyRef enemy) (attachTo aid enemy) | enemy <- targets]
        "40107" -> do
          enemies <- selectList (EnemyWithTrait Marauder)
          named <- filterM (\enemy -> getsGame $ \g -> maybe False ((`elem` ["Greycrow","Harpoon"]) . display . cdName) (abilityCardDef g $ enemyRef enemy)) enemies
          targets <- if notNull named then pure named else do
            powers <- for enemies \enemy -> (enemy,) <$> characterAttackValue (toCharacter enemy)
            let lowest = minimum $ map snd powers
            pure [enemy | (enemy,power) <- powers, power == lowest]
          first <- selectJust FirstPlayer
          if null targets then removeFromPlay a else chooseOrRunOne first [targeting (enemyRef enemy) (attachTo aid enemy) | enemy <- targets]
        _ -> pure ()
      pure e
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      for_ [(vid,you) | W.EnemyAttack (EnemyVillainId vid) you <- windows, attachmentEnemy a == Just (EnemyVillainId vid)] $ \(vid,you) -> do
        insteadOf (\case VillainMessage who (VillainBeginAttack _) -> who == vid; VillainMessage who (VillainBeginAttackWithoutBoost _) -> who == vid; _ -> False)
          (villainSchemesAgainst vid you)
      pure e
    RanAbility _ (isTarget a -> True) 2 _ _ -> do
      for_ [vid | Just (EnemyVillainId vid) <- [attachmentEnemy a]] $ \vid -> do
        cancelVillainDefeat vid
        villainStep vid SetVillainHp
        cardSpecific a "neCaptorDesperateReveal" ()
      pure $ OnTheRunAttachment a {attachmentCardDef = Cards.ne40105b}
    CardSpecific target "neCaptorDesperateReveal" _ | target == toRef a -> do
      main <- selectOne EncounterMainScheme
      for_ main $ \case SchemeMainSchemeId mid -> nextMainScheme mid; _ -> pure ()
      pure e
    AttachmentMessage aid (AttachmentDamaged n) | aid == attachmentId a, toCardCode a == "40106" ->
      pure $ OnTheRunAttachment a {attachmentDamage = attachmentDamage a + n}
    RanAbility _ (isTarget a -> True) 3 windows _ -> do
      for_ [(enemy,source,dmg) | W.EnemyDamageWouldBeDealt enemy source dmg <- windows, Just enemy == attachmentEnemy a] $ \(enemy,source,dmg) -> do
        cancelEnemyDamage enemy
        damageAttachment (attachmentId a) (damageAmount dmg)
        when (attachmentDamage a + damageAmount dmg >= 3) $ do
          actor <- getSourceController source
          for_ actor (cardSpecific a "neClutterAttack")
      pure e
    CardSpecific target "neClutterAttack" (fromJSON -> Success you) | target == toRef a -> do
      for_ (attachmentEnemy a) $ \enemy -> do
        stunned <- enemyMatches StunnedEnemy enemy
        forbidden <- elem CannotActivate <$> getModifiers (enemyRef enemy)
        when (not stunned && not forbidden) $ enemyActivationAfterCurrent enemy do
          enemyAttacks enemy you
          cardSpecific a "neClutterDiscardAfterAttack" ()
      pure e
    CardSpecific target "neClutterDiscardAfterAttack" _ | target == toRef a -> e <$ removeFromPlay a
    RanAbility _ (isTarget a -> True) 4 windows _ -> do
      when (any (\case W.DefendedWithoutDamage _ enemy -> Just enemy == attachmentEnemy a; _ -> False) windows) $ removeFromPlay a
      pure e
    _ -> OnTheRunAttachment <$> runMessage msg a

enemyRemainingHitPoints :: HasGame m => EnemyId -> m Natural
enemyRemainingHitPoints = getEnemyRemainingHitPoints
