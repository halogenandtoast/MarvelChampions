module Marvel.Attachment.Attachments.PumpkinBombs (
  pumpkinBombs,
  PumpkinBombs (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Entity
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Window

pumpkinBombs :: AttachmentCard PumpkinBombs
pumpkinBombs = attachment PumpkinBombs Cards.pumpkinBombs

newtype PumpkinBombs = PumpkinBombs (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | [star] Forced Response: after the villain attacks you, discard Pumpkin Bombs and take
-- 2 indirect damage. Hero Action: spend [physical] [physical] resources → discard this
-- card.
instance HasAbilities PumpkinBombs where
  getAbilities (PumpkinBombs a) = case attachmentEnemy a of
    Just enemy ->
      [ windowAbility a 1 (EnemyAttacked After (EnemyWithId enemy) AnyIdentity) ForcedResponse NoCost $ runAbility a 1
      , ability a 2 HeroAction NoCriteria (MultiResourceCost [Just Physical, Just Physical]) $ DiscardTarget (toRef a)
      ]
    Nothing -> []

instance RunMessage PumpkinBombs where
  runMessage msg a@(PumpkinBombs attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      villain <- selectJust ActiveVillain
      attachTo ident villain
      pure a
    RanAbility _ (isTarget a -> True) 1 [EnemyAttack _ you] _ -> do
      removeFromPlay attrs
      dealIndirectDamage you attrs 2
      pure a
    _ -> PumpkinBombs <$> runMessage msg attrs
