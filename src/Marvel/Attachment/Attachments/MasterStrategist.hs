module Marvel.Attachment.Attachments.MasterStrategist (
  masterStrategist,
  MasterStrategist (..),
) where

import Marvel.Prelude

import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Ability
import Marvel.Modifier
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref

masterStrategist :: AttachmentCard MasterStrategist
masterStrategist = attachment MasterStrategist Cards.masterStrategist

newtype MasterStrategist = MasterStrategist (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Forced Interrupt: when Red Skull activates, give him an additional boost card for
-- each side scheme in play, then discard this card. His activation is told by the boost
-- card it gives him, which is the first he is dealt in it.
instance RunMessage MasterStrategist where
  runMessage msg a@(MasterStrategist attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      villain <- selectJust ActiveVillain
      attachTo ident villain
      pure a
    DealBoost target | Just (EnemyVillainId vid) <- attachmentEnemy attrs, target `elem` [VillainRef vid, EnemyRef (EnemyVillainId vid)] -> do
      n <- selectListCount AnySideScheme
      -- discarded first, so the boost cards it gives are not told to it
      removeFromPlay attrs
      for_ [1 .. n] \_ -> dealBoost vid
      pure a
    _ -> MasterStrategist <$> runMessage msg attrs
