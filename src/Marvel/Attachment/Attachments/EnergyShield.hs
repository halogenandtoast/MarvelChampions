module Marvel.Attachment.Attachments.EnergyShield (
  energyShield,
  EnergyShield (..),
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

energyShield :: AttachmentCard EnergyShield
energyShield = attachment EnergyShield Cards.energyShield

newtype EnergyShield = EnergyShield (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | The attached villain gains retaliate 1.
instance HasModifiersFor EnergyShield where
  getModifiersFor _ (VillainRef vid) (EnergyShield a)
    | Just (EnemyVillainId vid) == attachmentEnemy a = pure [KeywordModifier $ Retaliate 1]
  getModifiersFor _ _ _ = pure []

-- | Hero Action: spend [energy][mental] resources → discard this card.
instance HasAbilities EnergyShield where
  getAbilities (EnergyShield a) = case attachmentEnemy a of
    Just _ -> [ability a 1 HeroAction NoCriteria (MultiResourceCost [Just Energy, Just Mental]) $ DiscardTarget (toRef a)]
    Nothing -> []

instance RunMessage EnergyShield where
  runMessage msg (EnergyShield attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      -- with no villain in play to attach to, it is discarded
      selectOne ActiveVillain >>= \case
        Nothing -> EnergyShield attrs <$ removeFromPlay attrs
        Just villainId -> do
          attachedTo villainId ident
          pure . EnergyShield $ attrs & enemyL ?~ EnemyVillainId villainId
    _ -> EnergyShield <$> runMessage msg attrs
