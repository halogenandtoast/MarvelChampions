module Marvel.Attachment.Attachments.Charge (
  charge,
  Charge (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Entity
import Marvel.Id
import Marvel.Keyword (Keyword (Overkill))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window

charge :: AttachmentCard Charge
charge = attachment Charge Cards.charge

newtype Charge = Charge (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Charge where
  getAbilities (Charge a) = case attachmentEnemy a of
    Just enemyId ->
      [ windowAbility
          a
          1
          (EnemyAttacked When (EnemyWithId enemyId) AnyIdentity)
          ForcedInterrupt
          NoCost
          $ runAbility a 1
      ]
    _ -> []

instance HasModifiersFor Charge where
  getModifiersFor _ (VillainRef vid) (Charge a)
    | Just (EnemyVillainId vid) == attachmentEnemy a = pure [AttackModifier 3]
  getModifiersFor _ _ _ = pure []

instance RunMessage Charge where
  runMessage msg a@(Charge attrs) = runQueueT $ case msg of
    AttachmentMessage ident msg' | ident == attachmentId attrs -> case msg' of
      RevealAttachment _ -> do
        -- with no villain in play to attach to, it is discarded
        selectOne ActiveVillain >>= \case
          Nothing -> Charge attrs <$ removeFromPlay attrs
          Just villainId -> do
            attachedTo villainId ident
            pure . Charge $ attrs & enemyL ?~ EnemyVillainId villainId
      _ -> Charge <$> runMessage msg attrs
    RanAbility _ (isTarget a -> True) 1 _ _ -> case attachmentEnemy attrs of
      Just (EnemyVillainId vid) -> do
        flip afterMatching (removeFromPlay a) \case
          VillainMessage vid' VillainEndAttack -> vid == vid'
          _ -> False
        attackGains vid Overkill
        pure a
      _ -> error "Invalid call"
    _ -> Charge <$> runMessage msg attrs
