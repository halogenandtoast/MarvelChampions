module Marvel.Environment.Types where

import Marvel.Prelude

import Data.HashSet qualified as HashSet
import Data.Typeable
import Marvel.Ability.Types
import Marvel.Card
import Marvel.Entity
import Marvel.Id hiding (EnvironmentId)
import Marvel.Id as X (EnvironmentId)
import Marvel.Message
import Marvel.Modifier
import Marvel.Ref

-- | An environment: an encounter card in the villain's play area whose rules hold while
-- it is in play.
data Environment = forall a. (IsEnvironment a) => Environment a

instance Show Environment where
  show (Environment a) = show a

instance ToJSON Environment where
  toJSON (Environment a) = toJSON a

instance Eq Environment where
  Environment (a :: a) == Environment (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

data SomeEnvironmentCard = forall a. (IsEnvironment a) => SomeEnvironmentCard (EnvironmentCard a)

liftEnvironmentCard :: (forall a. EnvironmentCard a -> b) -> SomeEnvironmentCard -> b
liftEnvironmentCard f (SomeEnvironmentCard a) = f a

someEnvironmentCardCode :: SomeEnvironmentCard -> CardCode
someEnvironmentCardCode = liftEnvironmentCard toCardCode

instance Entity Environment where
  type Id Environment = EnvironmentId
  data Attrs Environment = EnvironmentAttrs
    { environmentId :: EnvironmentId
    , environmentCardDef :: CardDef
    , environmentAttachments :: HashSet AttachmentId
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
  data Field Environment :: Type -> Type where
    EnvironmentId :: Field Environment EnvironmentId
    EnvironmentCardDef :: Field Environment CardDef
    EnvironmentAttachments :: Field Environment (HashSet AttachmentId)
  field fld e =
    let EnvironmentAttrs {..} = toAttrs e
     in case fld of
          EnvironmentId -> environmentId
          EnvironmentCardDef -> environmentCardDef
          EnvironmentAttachments -> environmentAttachments
  toId = environmentId . toAttrs
  toAttrs (Environment a) = toEnvironmentAttrs a

instance RunMessage Environment where
  runMessage msg (Environment a) = Environment <$> runMessage msg a

instance HasAbilities Environment where
  getAbilities (Environment a) = getAbilities a

instance HasModifiersFor Environment where
  getModifiersFor source target (Environment a) = getModifiersFor source target a

instance IsRef Environment where
  toRef = EnvironmentRef . toId

instance IsCard Environment where
  toCard = toCard . toAttrs

instance HasCardDef Environment where
  getCardDef = getCardDef . toAttrs

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, HasAbilities a, HasModifiersFor a, RunMessage a) => IsEnvironment a where
  toEnvironmentAttrs :: a -> Attrs Environment
  default toEnvironmentAttrs :: (Coercible a (Attrs Environment)) => a -> Attrs Environment
  toEnvironmentAttrs = coerce

type EnvironmentCard a = CardBuilder EnvironmentId a

environment :: (Attrs Environment -> a) -> CardDef -> CardBuilder EnvironmentId a
environment f cardDef =
  CardBuilder
    { cbCardDef = cardDef
    , cbCardBuilder = \cardDef eid -> f $ EnvironmentAttrs eid cardDef mempty
    }

attachmentsL :: Lens' (Attrs Environment) (HashSet AttachmentId)
attachmentsL = lens environmentAttachments $ \m x -> m {environmentAttachments = x}

instance IsRef (Attrs Environment) where
  toRef = EnvironmentRef . environmentId

instance HasCardCode (Attrs Environment) where
  toCardCode = toCardCode . environmentCardDef

instance HasCardDef (Attrs Environment) where
  getCardDef = environmentCardDef

instance IsCard (Attrs Environment) where
  toCard a =
    EncounterCard $
      MkEncounterCard
        { ecCardId = CardId $ unEnvironmentId $ environmentId a
        , ecCardDef = getCardDef a
        }

instance RunMessage (Attrs Environment) where
  runMessage msg attrs = case msg of
    EnvironmentMessage ident (AttachedToEnvironment attachmentId)
      | ident == environmentId attrs ->
          pure $ attrs & attachmentsL %~ HashSet.insert attachmentId
    AttachmentRemoved attachmentId ->
      pure $ attrs & attachmentsL %~ HashSet.delete attachmentId
    _ -> pure attrs
