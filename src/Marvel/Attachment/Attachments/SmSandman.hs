module Marvel.Attachment.Attachments.SmSandman where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Entity
import Marvel.Environment.Environments.SmSandman (surgingSands)
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window hiding (VillainDamaged)
import Marvel.Window qualified as W

newtype SmSandmanAttachment = SmSandmanAttachment (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

smSandForm :: AttachmentCard SmSandmanAttachment
smSandForm = attachment SmSandmanAttachment Cards.smSandForm

instance HasAbilities SmSandmanAttachment where
  getAbilities (SmSandmanAttachment a) = case attachmentEnemy a of
    Just (EnemyVillainId vid) -> [windowAbility a 1 (VillainWouldTakeDamage (VillainWithId vid) AnyDamage) ForcedInterrupt NoCost (runAbility a 1)]
    _ -> []

instance RunMessage SmSandmanAttachment where
  runMessage msg e@(SmSandmanAttachment attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment _) | aid == attachmentId attrs -> e <$ attachToTheVillain attrs
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [vid | W.DamagedVillain vid _ <- windows] \vid ->
        insteadOf (\case VillainMessage other (VillainDamaged _ _) -> other == vid; _ -> False) do
          removeFromPlay attrs
          surgingSands
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> e <$ (revealBoost attrs =<< getBoostedPlayer enemy)
    _ -> SmSandmanAttachment <$> runMessage msg attrs
