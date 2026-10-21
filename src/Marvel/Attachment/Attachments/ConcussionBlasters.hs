module Marvel.Attachment.Attachments.ConcussionBlasters (
  concussionBlasters,
  ConcussionBlasters (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Resource

concussionBlasters :: AttachmentCard ConcussionBlasters
concussionBlasters = attachment ConcussionBlasters Cards.concussionBlasters

newtype ConcussionBlasters = ConcussionBlasters (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor ConcussionBlasters where
  getModifiersFor _ (VillainRef vid) (ConcussionBlasters a)
    | Just (EnemyVillainId vid) == attachmentEnemy a = pure [KeywordModifier $ Retaliate 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities ConcussionBlasters where
  getAbilities (ConcussionBlasters a) =
    [ ability
        a
        1
        HeroAction
        NoCriteria
        (MultiResourceCost [Just Energy, Just Energy] <> ExhaustCost)
        (TargetLabel (toRef a) [DiscardTarget $ toRef a])
    ]

instance RunMessage ConcussionBlasters where
  runMessage msg a@(ConcussionBlasters attrs) = case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      villainId <- selectJust ActiveVillain
      push $ VillainMessage villainId $ AttachedToVillain ident
      pure . ConcussionBlasters $ attrs & enemyL ?~ EnemyVillainId villainId
    _ -> ConcussionBlasters <$> runMessage msg attrs
