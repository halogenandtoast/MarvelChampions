module Marvel.Treachery.Types where

import Marvel.Prelude

import Data.Typeable
import Marvel.Card
import Marvel.Entity
import Marvel.Id hiding (TreacheryId)
import Marvel.Id as X (TreacheryId)
import Marvel.Message
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Text.Show qualified

data Treachery = forall a . IsTreachery a => Treachery a

instance Show Treachery where
  show (Treachery a) = show a

instance Eq Treachery where
  Treachery (a :: a) == Treachery (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance ToJSON Treachery where
  toJSON (Treachery a) = toJSON a

data SomeTreacheryCard = forall a . IsTreachery a => SomeTreacheryCard
  (TreacheryCard a)

liftTreacheryCard
  :: (forall a . TreacheryCard a -> b) -> SomeTreacheryCard -> b
liftTreacheryCard f (SomeTreacheryCard a) = f a

someTreacheryCardCode :: SomeTreacheryCard -> CardCode
someTreacheryCardCode = liftTreacheryCard cbCardCode

instance Entity Treachery where
  type Id Treachery = TreacheryId
  data Attrs Treachery = TreacheryAttrs
    { treacheryId :: TreacheryId
    , treacheryCardDef :: CardDef
    , treacherySurge :: Bool
    , treacheryResolver :: Maybe IdentityId
    -- used when we need to remember for other messages
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
  data Field Treachery :: Type -> Type where
    TreacheryId :: Field Treachery TreacheryId
    TreacheryCardDef :: Field Treachery CardDef
    TreacherySurge :: Field Treachery Bool
    TreacheryResolver :: Field Treachery (Maybe IdentityId)
  field fld t =
    let TreacheryAttrs {..} = toAttrs t
    in
      case fld of
        TreacheryId -> treacheryId
        TreacheryCardDef -> treacheryCardDef
        TreacherySurge -> treacherySurge
        TreacheryResolver -> treacheryResolver
  toId = treacheryId . toAttrs
  toAttrs (Treachery a) = toTreacheryAttrs a

instance RunMessage Treachery where
  runMessage msg (Treachery a) = Treachery <$> runMessage msg a

instance IsSource Treachery where
  toSource = TreacherySource . toId

instance IsTarget Treachery where
  toTarget = TreacheryTarget . toId

instance IsCard Treachery where
  toCard = toCard . toAttrs

instance HasCardDef Treachery where
  getCardDef = getCardDef . toAttrs

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, RunMessage a) => IsTreachery a where
  toTreacheryAttrs :: a -> Attrs Treachery
  default toTreacheryAttrs :: Coercible a (Attrs Treachery) => a -> Attrs Treachery
  toTreacheryAttrs = coerce

type TreacheryCard a = CardBuilder TreacheryId a

surgeL :: Lens' (Attrs Treachery) Bool
surgeL = lens treacherySurge $ \m x -> m { treacherySurge = x }

resolverL :: Lens' (Attrs Treachery) (Maybe IdentityId)
resolverL = lens treacheryResolver $ \m x -> m { treacheryResolver = x }

instance HasCardCode (Attrs Treachery) where
  toCardCode = toCardCode . treacheryCardDef

treacheryWith
  :: (Attrs Treachery -> a)
  -> CardDef
  -> (Attrs Treachery -> Attrs Treachery)
  -> CardBuilder TreacheryId a
treacheryWith f cardDef g = treachery (f . g) cardDef

treachery :: (Attrs Treachery -> a) -> CardDef -> CardBuilder TreacheryId a
treachery f cardDef = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \mid -> f $ TreacheryAttrs
    { treacheryId = mid
    , treacheryCardDef = cardDef
    , treacherySurge = False
    , treacheryResolver = Nothing
    }
  }

instance IsSource (Attrs Treachery) where
  toSource = TreacherySource . treacheryId

instance IsTarget (Attrs Treachery) where
  toTarget = TreacheryTarget . treacheryId

instance IsCard (Attrs Treachery) where
  toCard a = EncounterCard $ MkEncounterCard
    { ecCardId = CardId $ unTreacheryId $ treacheryId a
    , ecCardDef = getCardDef a
    }

instance HasCardDef (Attrs Treachery) where
  getCardDef = treacheryCardDef

instance RunMessage (Attrs Treachery) where
  runMessage msg attrs = case msg of
    TreacheryMessage ident msg' | ident == treacheryId attrs -> case msg' of
      ResolvedTreachery identityId -> do
        pushAll
          $ RemoveFromPlay (toTarget attrs)
          : [ Surge identityId | treacherySurge attrs ]
        pure attrs
      _ -> pure attrs
    _ -> pure attrs
