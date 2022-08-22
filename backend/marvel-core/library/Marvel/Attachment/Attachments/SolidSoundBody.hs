module Marvel.Attachment.Attachments.SolidSoundBody
  ( solidSoundBody
  , SolidSoundBody(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Resource
import Marvel.Source
import Marvel.Target

solidSoundBody :: AttachmentCard SolidSoundBody
solidSoundBody = attachment SolidSoundBody Cards.solidSoundBody

newtype SolidSoundBody = SolidSoundBody AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasModifiersFor SolidSoundBody where
  getModifiersFor _ (VillainTarget vid) (SolidSoundBody a)
    | Just (EnemyVillainId vid) == attachmentEnemy a = pure
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
          $ DiscardTarget (toTarget a)
      ]
    _ -> []

instance RunMessage SolidSoundBody where
  runMessage msg (SolidSoundBody attrs) = case msg of
    AttachmentMessage aid msg' | aid == toId attrs -> case msg' of
      RevealAttachment _ -> do
        villainId <- selectJust ActiveVillain
        push $ VillainMessage villainId $ AttachedToVillain aid
        pure . SolidSoundBody $ attrs & enemyL ?~ EnemyVillainId villainId
      _ -> SolidSoundBody <$> runMessage msg attrs
    _ -> SolidSoundBody <$> runMessage msg attrs
