module Marvel.Attachment.Attachments.SmVenom where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id ()
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Window

newtype SmVenomAttachment = SmVenomAttachment (Attrs Attachment)
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

smNowWeReAngry :: AttachmentCard SmVenomAttachment
smNowWeReAngry = attachment SmVenomAttachment Cards.smNowWeReAngry

instance HasModifiersFor SmVenomAttachment where
  getModifiersFor _ target (SmVenomAttachment a) = pure
    [modifier | Just enemy <- [attachmentEnemy a], target == enemyRef enemy, modifier <- [AttackModifier 1, KeywordModifier Overkill]]

instance HasAbilities SmVenomAttachment where
  getAbilities (SmVenomAttachment a) =
    [windowAbility a 1 (EnemyTookDamage (EnemyWithId enemy) AnyAttack) ForcedResponse NoCost (runAbility a 1) | Just enemy <- [attachmentEnemy a]]

instance RunMessage SmVenomAttachment where
  runMessage msg e@(SmVenomAttachment attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment _) | aid == attachmentId attrs -> do
      attachToTheVillain attrs
      placeCounters attrs 2
      pure e
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> e <$ removeCounters attrs 1
    RemoveCounters target n | target == toRef attrs -> do
      when (attachmentCounters attrs <= n) $ removeFromPlay attrs
      SmVenomAttachment <$> runMessage msg attrs
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> e <$ (revealBoost attrs =<< getBoostedPlayer enemy)
    _ -> SmVenomAttachment <$> runMessage msg attrs
