module Marvel.Attachment.Attachments.EnhancedIvoryHorn
  ( enhancedIvoryHorn
  , EnhancedIvoryHorn(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Attrs
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Resource
import Marvel.Source
import Marvel.Target

enhancedIvoryHorn :: AttachmentCard EnhancedIvoryHorn
enhancedIvoryHorn = attachment EnhancedIvoryHorn Cards.enhancedIvoryHorn

newtype EnhancedIvoryHorn = EnhancedIvoryHorn AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasModifiersFor EnhancedIvoryHorn where
  getModifiersFor _ (VillainTarget vid) (EnhancedIvoryHorn a)
    | Just (EnemyVillainId vid) == attachmentEnemy a = pure [AttackModifier 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities EnhancedIvoryHorn where
  getAbilities (EnhancedIvoryHorn a) =
    [ ability
        a
        1
        HeroAction
        NoCriteria
        (MultiResourceCost [Just Physical, Just Physical, Just Physical])
        (TargetLabel (toTarget a) [DiscardTarget $ toTarget a])
    ]

instance RunMessage EnhancedIvoryHorn where
  runMessage msg (EnhancedIvoryHorn attrs) = case msg of
    AttachmentMessage aid msg' | aid == toId attrs -> case msg' of
      RevealAttachment _ -> do
        villainId <- selectJust ActiveVillain
        push $ VillainMessage villainId $ AttachedToVillain aid
        pure . EnhancedIvoryHorn $ attrs & enemyL ?~ EnemyVillainId villainId
      _ -> EnhancedIvoryHorn <$> runMessage msg attrs
    _ -> EnhancedIvoryHorn <$> runMessage msg attrs
