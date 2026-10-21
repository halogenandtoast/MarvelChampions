module Marvel.Attachment.Attachments.EnhancedIvoryHorn (
  enhancedIvoryHorn,
  EnhancedIvoryHorn (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Resource

enhancedIvoryHorn :: AttachmentCard EnhancedIvoryHorn
enhancedIvoryHorn = attachment EnhancedIvoryHorn Cards.enhancedIvoryHorn

newtype EnhancedIvoryHorn = EnhancedIvoryHorn (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor EnhancedIvoryHorn where
  getModifiersFor _ (VillainRef vid) (EnhancedIvoryHorn a)
    | Just (EnemyVillainId vid) == attachmentEnemy a = pure [AttackModifier 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities EnhancedIvoryHorn where
  getAbilities a =
    [ ability
        a
        1
        HeroAction
        NoCriteria
        (MultiResourceCost [Just Physical, Just Physical, Just Physical])
        (TargetLabel (toRef a) [DiscardTarget $ toRef a])
    ]

instance RunMessage EnhancedIvoryHorn where
  runMessage msg (EnhancedIvoryHorn attrs) = case msg of
    AttachmentMessage ident msg' | ident == attachmentId attrs -> case msg' of
      RevealAttachment _ -> do
        villainId <- selectJust ActiveVillain
        push $ VillainMessage villainId $ AttachedToVillain ident
        pure . EnhancedIvoryHorn $ attrs & enemyL ?~ EnemyVillainId villainId
      _ -> EnhancedIvoryHorn <$> runMessage msg attrs
    _ -> EnhancedIvoryHorn <$> runMessage msg attrs
