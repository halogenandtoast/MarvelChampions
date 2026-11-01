module Marvel.Attachment.Attachments.AgeOfApocalypseEnSabahNur where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question
import Marvel.Scenario.AgeOfApocalypseEnSabahNur
import Marvel.Window qualified as W
import Marvel.Card
import Marvel.Entity
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Ability ()
import Marvel.Modifier ()
import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype AoaEnSabahNurAttachment = AoaEnSabahNurAttachment (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

staggeringStrength :: AttachmentCard AoaEnSabahNurAttachment
staggeringStrength = attachment AoaEnSabahNurAttachment Cards.staggeringStrength

instance HasModifiersFor AoaEnSabahNurAttachment where
  getModifiersFor _ target (AoaEnSabahNurAttachment attrs) = pure [AttackModifier 2 | Just target == (enemyRef <$> attachmentEnemy attrs)]

instance HasAbilities AoaEnSabahNurAttachment where
  getAbilities a@(AoaEnSabahNurAttachment attrs) =
    [limitedWindowAbility a 1 (W.EnemyAttacked W.When (EnemyWithId enemy) AnyIdentity)
      ForcedInterrupt NoCriteria NoCost (runAbility a 1) | Just enemy <- [attachmentEnemy attrs]]
instance RunMessage AoaEnSabahNurAttachment where
  runMessage msg a@(AoaEnSabahNurAttachment attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment _) | aid == attachmentId attrs -> do
      current <- enSabahNur
      for_ current $ attachTo aid
      changeApocalypseForm "Giant"
      pure a
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ ([you | W.EnemyAttack _ you <- windows] <> [you | W.EnemyAttackAlly _ _ you <- windows]) stunIdentity
      for_ (attachmentEnemy attrs) $ \enemy -> enemyActivationAfterCurrent enemy $ removeFromPlay attrs
      pure a
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      card <- getBoostCard target
      for_ card $ \encounter -> do
        consumeBoostCard encounter
        enemyActivationAfterCurrent enemy $ revealEncounterCard you encounter
      pure a
    _ -> AoaEnSabahNurAttachment <$> runMessage msg attrs
