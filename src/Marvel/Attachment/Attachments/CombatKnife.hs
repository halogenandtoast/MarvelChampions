module Marvel.Attachment.Attachments.CombatKnife (
  combatKnife,
  CombatKnife (..),
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
import Marvel.Keyword (Keyword (Piercing))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource

combatKnife :: AttachmentCard CombatKnife
combatKnife = attachment CombatKnife Cards.combatKnife

newtype CombatKnife = CombatKnife (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | +1 ATK, and [star] the attached villain's attacks gain piercing.
instance HasModifiersFor CombatKnife where
  getModifiersFor _ (VillainRef vid) (CombatKnife a)
    | Just (EnemyVillainId vid) == attachmentEnemy a = pure [AttackModifier 1, KeywordModifier Piercing]
  getModifiersFor _ _ _ = pure []

-- | Hero Action: spend [mental][physical] resources → discard this card.
instance HasAbilities CombatKnife where
  getAbilities (CombatKnife a) = case attachmentEnemy a of
    Just _ -> [ability a 1 HeroAction NoCriteria (MultiResourceCost [Just Mental, Just Physical]) $ DiscardTarget (toRef a)]
    Nothing -> []

instance RunMessage CombatKnife where
  runMessage msg (CombatKnife attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      villainId <- selectJust ActiveVillain
      attachedTo villainId ident
      pure . CombatKnife $ attrs & enemyL ?~ EnemyVillainId villainId
    _ -> CombatKnife <$> runMessage msg attrs
