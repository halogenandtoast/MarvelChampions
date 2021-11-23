module Marvel.Attachment.Attachments.Charge
  ( charge
  , Charge(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Attrs
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Window

charge :: AttachmentCard Charge
charge = attachment Charge Cards.charge

newtype Charge = Charge AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities Charge where
  getAbilities (Charge a) = case attachmentEnemy a of
    Just enemyId ->
      [ windowAbility
            a
            1
            (EnemyAttacked (EnemyWithId enemyId) AnyIdentity)
            ForcedInterrupt
            NoCost
          $ RunAbility (toTarget a) 1
      ]
    _ -> []

instance HasModifiersFor Charge where
  getModifiersFor _ (VillainTarget vid) (Charge a)
    | Just (EnemyVillainId vid) == attachmentEnemy a = pure [AttackModifier 3]
  getModifiersFor _ _ _ = pure []

instance RunMessage Charge where
  runMessage msg a@(Charge attrs) = case msg of
    AttachmentMessage aid msg' | aid == toId attrs -> case msg' of
      RevealAttachment -> do
        villainId <- selectJust ActiveVillain
        push $ VillainMessage villainId $ AttachedToVillain aid
        pure a
      _ -> pure a
    RanAbility target 1 _ | isTarget attrs target ->
      case attachmentEnemy attrs of
        Just (EnemyVillainId vid) -> do
          replaceMatchingMessage
              [VillainMessage vid VillainEndAttack, RemoveFromPlay target]
            $ \case
                VillainMessage vid' VillainEndAttack -> vid == vid'
                _ -> False
          pushAll [VillainMessage vid VillainAttackGainOverkill]
          pure a
        _ -> error "Invalid call"
    _ -> Charge <$> runMessage msg attrs
