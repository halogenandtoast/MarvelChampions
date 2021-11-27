{-# LANGUAGE TemplateHaskell #-}
module Marvel.Attachment.Attrs where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Queue
import Marvel.Source
import Marvel.Target

class IsAttachment a

type AttachmentCard a = CardBuilder AttachmentId a

data AttachmentAttrs = AttachmentAttrs
  { attachmentId :: AttachmentId
  , attachmentCardDef :: CardDef
  , attachmentEnemy :: Maybe EnemyId
  , attachmentDamage :: Natural
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeLensesWith suffixedFields ''AttachmentAttrs

instance HasCardCode AttachmentAttrs where
  toCardCode = toCardCode . attachmentCardDef

attachment :: (AttachmentAttrs -> a) -> CardDef -> CardBuilder AttachmentId a
attachment f cardDef = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \mid -> f $ AttachmentAttrs
    { attachmentId = mid
    , attachmentCardDef = cardDef
    , attachmentEnemy = Nothing
    , attachmentDamage = 0
    }
  }

instance Entity AttachmentAttrs where
  type EntityId AttachmentAttrs = AttachmentId
  type EntityAttrs AttachmentAttrs = AttachmentAttrs
  toId = attachmentId
  toAttrs = id

instance IsSource AttachmentAttrs where
  toSource = AttachmentSource . toId

instance IsTarget AttachmentAttrs where
  toTarget = AttachmentTarget . toId

instance RunMessage AttachmentAttrs where
  runMessage msg attrs = case msg of
    AttachmentMessage attachmentId msg' | attachmentId == toId attrs ->
      case msg' of
        AttachedToEnemy enemyId -> do
          case enemyId of
            EnemyMinionId minionId -> do
              push (MinionMessage minionId $ AttachedToMinion $ toId attrs)
            EnemyVillainId villainId -> do
              push (VillainMessage villainId $ AttachedToVillain $ toId attrs)
          pure $ attrs & enemyL ?~ enemyId
        _ -> pure attrs
    _ -> pure attrs

instance HasCardDef AttachmentAttrs where
  getCardDef = attachmentCardDef
