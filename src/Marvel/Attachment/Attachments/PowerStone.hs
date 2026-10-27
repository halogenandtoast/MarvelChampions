module Marvel.Attachment.Attachments.PowerStone (
  powerStone,
  PowerStone (..),
) where

import Marvel.Prelude

import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Ability
import Marvel.Damage
import Marvel.Modifier
import Marvel.Id
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Window qualified as W

powerStone :: AttachmentCard PowerStone
powerStone = attachment PowerStone Cards.powerStone

-- | Setup. Attach to the villain. Permanent (Power Stone): what it is on leaving play, it
-- attaches to the villain again, and with none is removed from the game
-- ('Marvel.Attachment.Types'). Forced Response: after a hero or villain deals 3 or more
-- damage to attached character with a single attack, attach Power Stone to the attacking
-- hero or villain.
newtype PowerStone = PowerStone (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PowerStone where
  runMessage msg a@(PowerStone attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      attachToTheVillain attrs
      pure a
    -- a villain's attack on the hero holding it
    (attackDamageWindow -> Just (EnemyVillainId villain, IdentityCharacter hero, n))
      | n >= 3, attachmentIdentity attrs == Just hero -> do
          attachTo (attachmentId attrs) villain
          pure a
    -- a hero's attack on the villain holding it: an ally's is not a hero's
    CheckWindows [W.Window W.After (W.VillainTookDamage villain _ damage)]
      | damageAmount damage >= 3
      , attachmentEnemy attrs == Just (EnemyVillainId villain)
      , FromPlayerAttack hero <- damageSource damage -> do
          attachToIdentity (attachmentId attrs) hero
          pure a
    _ -> PowerStone <$> runMessage msg attrs
