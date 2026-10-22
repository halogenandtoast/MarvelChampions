module Marvel.Attachment.Attachments.CrossfiresRifle (
  crossfiresRifle,
  CrossfiresRifle (..),
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
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource

crossfiresRifle :: AttachmentCard CrossfiresRifle
crossfiresRifle = attachment CrossfiresRifle Cards.crossfiresRifle

newtype CrossfiresRifle = CrossfiresRifle (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | +2 ATK, and "[star] When attached enemy attacks, the attack gains ranged": its
-- attacks have the keyword.
instance HasModifiersFor CrossfiresRifle where
  getModifiersFor _ target (CrossfiresRifle a)
    | Just enemy <- attachmentEnemy a, isEnemy enemy = pure [AttackModifier 2, KeywordModifier Ranged]
   where
    isEnemy = \case
      EnemyVillainId vid -> target == VillainRef vid
      EnemyMinionId mid -> target == MinionRef mid
  getModifiersFor _ _ _ = pure []

-- | Hero Action: Exhaust your hero and spend a [wild] resource → discard Crossfire's
-- Rifle.
instance HasAbilities CrossfiresRifle where
  getAbilities (CrossfiresRifle a) =
    [ ability a 1 HeroAction NoCriteria (ExhaustCost <> ResourceCost (Just Wild)) $
        TargetLabel (toRef a) [DiscardTarget $ toRef a]
    ]

-- | Attach to Crossfire. Otherwise, attach to the villain.
instance RunMessage CrossfiresRifle where
  runMessage msg (CrossfiresRifle attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      crossfire <- selectOne $ MinionIs Minions.crossfire
      enemy <- maybe (EnemyVillainId <$> selectJust ActiveVillain) (pure . EnemyMinionId) crossfire
      attachedTo enemy ident
      pure . CrossfiresRifle $ attrs & enemyL ?~ enemy
    _ -> CrossfiresRifle <$> runMessage msg attrs
