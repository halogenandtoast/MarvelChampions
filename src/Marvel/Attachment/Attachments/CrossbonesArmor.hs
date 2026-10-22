module Marvel.Attachment.Attachments.CrossbonesArmor (
  crossbonesArmor,
  CrossbonesArmor (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Damage
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W

crossbonesArmor :: AttachmentCard CrossbonesArmor
crossbonesArmor = attachment CrossbonesArmor Cards.crossbonesArmor

newtype CrossbonesArmor = CrossbonesArmor (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | +1 ATK and +1 SCH to Crossbones.
instance HasModifiersFor CrossbonesArmor where
  getModifiersFor _ (VillainRef vid) (CrossbonesArmor a)
    | Just (EnemyVillainId vid) == attachmentEnemy a = pure [AttackModifier 1, SchemeModifier 1]
  getModifiersFor _ _ _ = pure []

-- | Forced Interrupt: when Crossbones would take any amount of damage, place it here
-- instead.
instance HasAbilities CrossbonesArmor where
  getAbilities (CrossbonesArmor a) = case attachmentEnemy a of
    Just (EnemyVillainId vid) ->
      [windowAbility a 1 (W.VillainDamaged W.When $ VillainWithId vid) ForcedInterrupt NoCost $ runAbility a 1]
    _ -> []

damaged :: [W.WindowType] -> Maybe (VillainId, Natural)
damaged = \case
  W.DamagedVillain vid n : _ -> Just (vid, damageAmount n)
  _ : ws -> damaged ws
  [] -> Nothing

-- | Attach to Crossbones. With 5 or more damage here, it is discarded.
instance RunMessage CrossbonesArmor where
  runMessage msg a@(CrossbonesArmor attrs) = runQueueT $ case msg of
    AttachmentMessage ident msg' | ident == attachmentId attrs -> case msg' of
      RevealAttachment _ -> do
        -- with no villain in play to attach to, it is discarded
        selectOne ActiveVillain >>= \case
          Nothing -> CrossbonesArmor attrs <$ removeFromPlay attrs
          Just villainId -> do
            attachedTo villainId ident
            pure . CrossbonesArmor $ attrs & enemyL ?~ EnemyVillainId villainId
      AttachmentDamaged n -> do
        when (attachmentDamage attrs + n >= 5) $ removeFromPlay a
        pure . CrossbonesArmor $ attrs & damageL +~ n
      _ -> CrossbonesArmor <$> runMessage msg attrs
    RanAbility _ (isTarget a -> True) 1 (damaged -> Just (vid, dmg)) _ -> do
      flip insteadOf (damageAttachment (attachmentId attrs) dmg) \case
        VillainMessage vid' (VillainDamaged _ _) -> vid == vid'
        _ -> False
      pure a
    _ -> CrossbonesArmor <$> runMessage msg attrs
