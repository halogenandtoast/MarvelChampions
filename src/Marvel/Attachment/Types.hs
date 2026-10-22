module Marvel.Attachment.Types where

import Marvel.Prelude

import Data.Typeable
import Marvel.Ability.Types
import Marvel.Card
import Marvel.Entity
import Marvel.Id hiding (AttachmentId)
import Marvel.Id as X (AttachmentId)
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Matchers.Types (VillainMatcher (ActiveVillain))
import Marvel.Query (selectOne)

data Attachment = forall a. (IsAttachment a) => Attachment a

instance Show Attachment where
  show (Attachment a) = show a

instance ToJSON Attachment where
  toJSON (Attachment a) = toJSON a

instance Eq Attachment where
  Attachment (a :: a) == Attachment (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

data SomeAttachmentCard
  = forall a.
    (IsAttachment a) =>
    SomeAttachmentCard
      (AttachmentCard a)

liftAttachmentCard ::
  (forall a. AttachmentCard a -> b) -> SomeAttachmentCard -> b
liftAttachmentCard f (SomeAttachmentCard a) = f a

someAttachmentCardCode :: SomeAttachmentCard -> CardCode
someAttachmentCardCode = liftAttachmentCard toCardCode

instance Entity Attachment where
  type Id Attachment = AttachmentId
  data Attrs Attachment = AttachmentAttrs
    { attachmentId :: AttachmentId
    , attachmentCardDef :: CardDef
    , attachmentEnemy :: Maybe EnemyId
    , attachmentDamage :: Natural
    , attachmentIdentity :: Maybe IdentityId
    -- ^ the hero it is attached to, if it is
    , attachmentCounters :: Natural
    , attachmentSideScheme :: Maybe SideSchemeId
    -- ^ the side scheme it is attached to, if it is
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
  data Field Attachment :: Type -> Type where
    AttachmentId :: Field Attachment AttachmentId
    AttachmentCardDef :: Field Attachment CardDef
    AttachmentEnemy :: Field Attachment (Maybe EnemyId)
    AttachmentDamage :: Field Attachment Natural
    AttachmentCounters :: Field Attachment Natural
  field fld a =
    let AttachmentAttrs {..} = toAttrs a
     in case fld of
          AttachmentId -> attachmentId
          AttachmentCardDef -> attachmentCardDef
          AttachmentEnemy -> attachmentEnemy
          AttachmentDamage -> attachmentDamage
          AttachmentCounters -> attachmentCounters
  toId = attachmentId . toAttrs
  toAttrs (Attachment a) = toAttachmentAttrs a

instance RunMessage Attachment where
  runMessage msg (Attachment a) = Attachment <$> runMessage msg a

instance IsRef Attachment where
  toRef = AttachmentRef . toId

instance HasAbilities Attachment where
  getAbilities (Attachment a) = getAbilities a

instance HasModifiersFor Attachment where
  getModifiersFor source target (Attachment a) =
    getModifiersFor source target a

instance IsCard Attachment where
  toCard = toCard . toAttrs

instance HasCardDef Attachment where
  getCardDef = getCardDef . toAttrs

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, HasAbilities a, HasModifiersFor a, RunMessage a) => IsAttachment a where
  toAttachmentAttrs :: a -> Attrs Attachment
  default toAttachmentAttrs :: (Coercible a (Attrs Attachment)) => a -> Attrs Attachment
  toAttachmentAttrs = coerce

type AttachmentCard a = CardBuilder AttachmentId a

enemyL :: Lens' (Attrs Attachment) (Maybe EnemyId)
enemyL = lens attachmentEnemy $ \m x -> m {attachmentEnemy = x}

damageL :: Lens' (Attrs Attachment) Natural
damageL = lens attachmentDamage $ \m x -> m {attachmentDamage = x}

countersL :: Lens' (Attrs Attachment) Natural
countersL = lens attachmentCounters $ \m x -> m {attachmentCounters = x}

instance HasCardCode (Attrs Attachment) where
  toCardCode = toCardCode . attachmentCardDef

attachment :: (Attrs Attachment -> a) -> CardDef -> CardBuilder AttachmentId a
attachment f cardDef =
  CardBuilder
    { cbCardDef = cardDef
    , cbCardBuilder = \cardDef mid ->
        f $
          AttachmentAttrs
            { attachmentId = mid
            , attachmentCardDef = cardDef
            , attachmentEnemy = Nothing
            , attachmentDamage = 0
            , attachmentIdentity = Nothing
            , attachmentCounters = 0
            , attachmentSideScheme = Nothing
            }
    }

instance IsRef (Attrs Attachment) where
  toRef = AttachmentRef . attachmentId

-- | "Attach to the villain": "your" area's villain, or, with none in play there (as
-- between one Kang and the next), the card is discarded.
attachToTheVillain :: (ReverseQueue m) => Attrs Attachment -> m ()
attachToTheVillain attrs = selectOne ActiveVillain >>= maybe (removeFromPlay attrs) (attachTo (attachmentId attrs))

instance RunMessage (Attrs Attachment) where
  runMessage msg attrs = runQueueT $ case msg of
    AttachmentMessage attachmentId' msg'
      | attachmentId' == attachmentId attrs -> case msg' of
          AttachedToEnemy enemyId -> do
            attachedTo enemyId (attachmentId attrs)
            pure $ attrs & enemyL ?~ enemyId
          AttachedToIdentity ident -> pure attrs {attachmentIdentity = Just ident}
          AttachedToSideScheme sid -> pure attrs {attachmentSideScheme = Just sid}
          _ -> pure attrs
    -- it leaves play with the card it is attached to
    RemoveFromPlay target
      | Just target `elem` [toTarget <$> attachmentIdentity attrs, toTarget <$> attachmentSideScheme attrs] -> do
          removeFromPlay attrs
          pure attrs
    PlaceCounters target n | target == toRef attrs -> pure $ attrs & countersL +~ n
    RemoveCounters target n | target == toRef attrs -> pure $ attrs & countersL %~ subtractNatural n
    _ -> pure attrs

instance IsCard (Attrs Attachment) where
  toCard a =
    EncounterCard $
      MkEncounterCard
        { ecCardId = CardId $ unAttachmentId $ attachmentId a
        , ecCardDef = getCardDef a
        }

instance HasCardDef (Attrs Attachment) where
  getCardDef = attachmentCardDef
