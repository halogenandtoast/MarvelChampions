module Marvel.Attachment.Types where

import Marvel.Prelude

import Data.Typeable
import Marvel.Ability.Type
import Marvel.Card
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Modifier
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Text.Show qualified

data Attachment = forall a . IsAttachment a => Attachment a

instance Show Attachment where
  show (Attachment a) = show a

instance ToJSON Attachment where
  toJSON (Attachment a) = toJSON a

instance Eq Attachment where
  (Attachment (a :: a)) == (Attachment (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

data SomeAttachmentCard = forall a . IsAttachment a => SomeAttachmentCard
  (AttachmentCard a)

liftAttachmentCard
  :: (forall a . AttachmentCard a -> b) -> SomeAttachmentCard -> b
liftAttachmentCard f (SomeAttachmentCard a) = f a

someAttachmentCardCode :: SomeAttachmentCard -> CardCode
someAttachmentCardCode = liftAttachmentCard cbCardCode

instance Entity Attachment where
  type EntityId Attachment = AttachmentId
  type EntityAttrs Attachment = AttachmentAttrs
  toId = toId . toAttrs
  toAttrs (Attachment a) = toAttrs a

instance RunMessage Attachment where
  runMessage msg (Attachment a) = Attachment <$> runMessage msg a

instance IsSource Attachment where
  toSource = AttachmentSource . toId

instance IsTarget Attachment where
  toTarget = AttachmentTarget . toId

instance HasAbilities Attachment where
  getAbilities (Attachment a) = getAbilities a

instance HasModifiersFor Attachment where
  getModifiersFor source target (Attachment a) =
    getModifiersFor source target a

instance IsCard Attachment where
  toCard = toCard . toAttrs

instance HasCardDef Attachment where
  getCardDef = getCardDef . toAttrs

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, Entity a, EntityAttrs a ~ AttachmentAttrs, EntityId a ~ AttachmentId, HasAbilities a, HasModifiersFor a, RunMessage a) => IsAttachment a

type AttachmentCard a = CardBuilder AttachmentId a

data AttachmentAttrs = AttachmentAttrs
  { attachmentId :: AttachmentId
  , attachmentCardDef :: CardDef
  , attachmentEnemy :: Maybe EnemyId
  , attachmentDamage :: Natural
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

enemyL :: Lens' AttachmentAttrs (Maybe EnemyId)
enemyL = lens attachmentEnemy $ \m x -> m { attachmentEnemy = x }

damageL :: Lens' AttachmentAttrs Natural
damageL = lens attachmentDamage $ \m x -> m { attachmentDamage = x }

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

instance IsCard AttachmentAttrs where
  toCard a = EncounterCard $ MkEncounterCard
    { ecCardId = CardId $ unAttachmentId $ toId a
    , ecCardDef = getCardDef a
    }

instance HasCardDef AttachmentAttrs where
  getCardDef = attachmentCardDef
