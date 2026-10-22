module Marvel.Attachment.Attachments.PowerGauntlets (
  powerGauntlets,
  PowerGauntlets (..),
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
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Window

powerGauntlets :: AttachmentCard PowerGauntlets
powerGauntlets = attachment PowerGauntlets Cards.powerGauntlets

newtype PowerGauntlets = PowerGauntlets (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | [star] Forced Response: after the attached villain attacks and damages you, discard 1
-- card from your hand. Hero Action: spend [mental][physical] resources → discard this
-- card.
instance HasAbilities PowerGauntlets where
  getAbilities (PowerGauntlets a) = case attachmentEnemy a of
    Just enemy ->
      [ windowAbility a 1 (EnemyAttackedAndDamaged (EnemyWithId enemy) (IdentityCharacters You)) ForcedResponse NoCost $
          runAbility a 1
      , ability a 2 HeroAction NoCriteria (MultiResourceCost [Just Mental, Just Physical]) $ DiscardTarget (toRef a)
      ]
    Nothing -> []

instance RunMessage PowerGauntlets where
  runMessage msg a@(PowerGauntlets attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      -- with no villain in play to attach to, it is discarded
      selectOne ActiveVillain >>= \case
        Nothing -> PowerGauntlets attrs <$ removeFromPlay attrs
        Just villainId -> do
          attachedTo villainId ident
          pure . PowerGauntlets $ attrs & enemyL ?~ EnemyVillainId villainId
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      discardFromHand you 1
      pure a
    _ -> PowerGauntlets <$> runMessage msg attrs
