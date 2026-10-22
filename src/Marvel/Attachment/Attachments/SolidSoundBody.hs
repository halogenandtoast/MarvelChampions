module Marvel.Attachment.Attachments.SolidSoundBody (
  solidSoundBody,
  SolidSoundBody (..),
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
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource

solidSoundBody :: AttachmentCard SolidSoundBody
solidSoundBody = attachment SolidSoundBody Cards.solidSoundBody

newtype SolidSoundBody = SolidSoundBody (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor SolidSoundBody where
  getModifiersFor _ (VillainRef vid) (SolidSoundBody a)
    | Just (EnemyVillainId vid) == attachmentEnemy a =
        pure
          [KeywordModifier $ Retaliate 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities SolidSoundBody where
  getAbilities (SolidSoundBody a) = case attachmentEnemy a of
    Just _ ->
      [ ability
          a
          1
          HeroAction
          NoCriteria
          (MultiResourceCost [Just Energy, Just Mental, Just Physical])
          $ DiscardTarget (toRef a)
      ]
    _ -> []

instance RunMessage SolidSoundBody where
  runMessage msg (SolidSoundBody attrs) = runQueueT $ case msg of
    AttachmentMessage ident msg' | ident == attachmentId attrs -> case msg' of
      RevealAttachment _ -> do
        -- with no villain in play to attach to, it is discarded
        selectOne ActiveVillain >>= \case
          Nothing -> SolidSoundBody attrs <$ removeFromPlay attrs
          Just villainId -> do
            attachedTo villainId ident
            pure . SolidSoundBody $ attrs & enemyL ?~ EnemyVillainId villainId
      _ -> SolidSoundBody <$> runMessage msg attrs
    _ -> SolidSoundBody <$> runMessage msg attrs
