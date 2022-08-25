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
  type Id Attachment = AttachmentId
  data Attrs Attachment = AttachmentAttrs
    { attachmentId :: AttachmentId
    , attachmentCardDef :: CardDef
    , attachmentEnemy :: Maybe EnemyId
    , attachmentDamage :: Natural
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
  data Field Attachment :: Type -> Type where
    AttachmentId :: Field Attachment AttachmentId
    AttachmentCardDef :: Field Attachment CardDef
    AttachmentEnemy :: Field Attachment (Maybe EnemyId)
    AttachmentDamage :: Field Attachment Natural
  toId = attachmentId . toAttrs
  toAttrs (Attachment a) = toAttachmentAttrs a

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

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, Entity a, Id a ~ AttachmentId, HasAbilities a, HasModifiersFor a, RunMessage a) => IsAttachment a where
  toAttachmentAttrs :: a -> Attrs Attachment
  default toAttachmentAttrs :: Coercible a (Attrs Attachment) => a -> Attrs Attachment
  toAttachmentAttrs = coerce

type AttachmentCard a = CardBuilder AttachmentId a

enemyL :: Lens' (Attrs Attachment) (Maybe EnemyId)
enemyL = lens attachmentEnemy $ \m x -> m { attachmentEnemy = x }

damageL :: Lens' (Attrs Attachment) Natural
damageL = lens attachmentDamage $ \m x -> m { attachmentDamage = x }

instance HasCardCode (Attrs Attachment) where
  toCardCode = toCardCode . attachmentCardDef

attachment :: (Attrs Attachment -> a) -> CardDef -> CardBuilder AttachmentId a
attachment f cardDef = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \mid -> f $ AttachmentAttrs
    { attachmentId = mid
    , attachmentCardDef = cardDef
    , attachmentEnemy = Nothing
    , attachmentDamage = 0
    }
  }

instance IsSource (Attrs Attachment) where
  toSource = AttachmentSource . attachmentId

instance IsTarget (Attrs Attachment) where
  toTarget = AttachmentTarget . attachmentId

instance RunMessage (Attrs Attachment) where
  runMessage msg attrs = case msg of
    AttachmentMessage attachmentId' msg' | attachmentId' == attachmentId attrs ->
      case msg' of
        AttachedToEnemy enemyId -> do
          case enemyId of
            EnemyMinionId minionId -> do
              push (MinionMessage minionId $ AttachedToMinion $ attachmentId attrs)
            EnemyVillainId villainId -> do
              push (VillainMessage villainId $ AttachedToVillain $ attachmentId attrs)
          pure $ attrs & enemyL ?~ enemyId
        _ -> pure attrs
    _ -> pure attrs

instance IsCard (Attrs Attachment) where
  toCard a = EncounterCard $ MkEncounterCard
    { ecCardId = CardId $ unAttachmentId $ attachmentId a
    , ecCardDef = getCardDef a
    }

instance HasCardDef (Attrs Attachment) where
  getCardDef = attachmentCardDef
