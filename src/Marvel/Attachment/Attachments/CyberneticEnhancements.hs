module Marvel.Attachment.Attachments.CyberneticEnhancements where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Trait qualified as Trait
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types

cyberneticEnhancements :: AttachmentCard CyberneticEnhancements
cyberneticEnhancements = attachment CyberneticEnhancements Cards.cyberneticEnhancements

newtype CyberneticEnhancements = CyberneticEnhancements (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor CyberneticEnhancements where
  getModifiersFor _ (MinionRef mid) (CyberneticEnhancements attrs)
    | attachmentEnemy attrs == Just (EnemyMinionId mid) = pure [AttackModifier 1, CannotTakeDamage]
  getModifiersFor _ _ _ = pure []

instance HasAbilities CyberneticEnhancements where
  getAbilities (CyberneticEnhancements attrs) = case attachmentEnemy attrs of
    Just enemy@(EnemyMinionId _) ->
      [windowAbility attrs 1 (W.EnemyAttackedAnyTarget W.After $ EnemyWithId enemy) ForcedResponse NoCost (runAbility attrs 1)]
    _ -> []

instance RunMessage CyberneticEnhancements where
  runMessage msg a@(CyberneticEnhancements attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      minions <- selectList AnyMinion
      if null minions
        then surge you >> removeFromPlay attrs
        else chooseOrRunOne you [targeting mid (attachTo aid mid) | mid <- minions]
      pure a
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> a <$ removeFromPlay attrs
    _ -> CyberneticEnhancements <$> runMessage msg attrs
