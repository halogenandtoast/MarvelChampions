module Marvel.Attachment.Attachments.SonicConverter (
  sonicConverter,
  SonicConverter (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Damage
import Marvel.Criteria
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource

sonicConverter :: AttachmentCard SonicConverter
sonicConverter = attachment SonicConverter Cards.sonicConverter

newtype SonicConverter = SonicConverter (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor SonicConverter where
  getModifiersFor _ (VillainRef vid) (SonicConverter a)
    | Just (EnemyVillainId vid) == attachmentEnemy a = pure [AttackModifier 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities SonicConverter where
  getAbilities (SonicConverter a) = case attachmentEnemy a of
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

instance RunMessage SonicConverter where
  runMessage msg a@(SonicConverter attrs) = runQueueT $ case msg of
    AttachmentMessage ident msg' | ident == attachmentId attrs -> case msg' of
      RevealAttachment _ -> do
        -- with no villain in play to attach to, it is discarded
        selectOne ActiveVillain >>= \case
          Nothing -> SonicConverter attrs <$ removeFromPlay attrs
          Just villainId -> do
            attachedTo villainId ident
            pure . SonicConverter $ attrs & enemyL ?~ EnemyVillainId villainId
      _ -> SonicConverter <$> runMessage msg attrs
    -- Forced Response: after Klaw attacks and damages a character, stun that character
    IdentityMessage ident (IdentityDamaged source damage) | byKlawsAttack source damage -> do
      stunIdentity ident
      pure a
    AllyMessage ident (AllyDamaged source damage) | byKlawsAttack source damage -> do
      stunAlly ident
      pure a
    _ -> SonicConverter <$> runMessage msg attrs
   where
    byKlawsAttack source damage = case (source, attachmentEnemy attrs) of
      (VillainRef vid, Just (EnemyVillainId vid')) ->
        vid == vid' && damageSource damage == FromAttack && damageAmount damage > 0
      _ -> False
