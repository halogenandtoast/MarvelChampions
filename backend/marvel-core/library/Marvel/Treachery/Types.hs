module Marvel.Treachery.Types where

import Marvel.Prelude

import Data.Typeable
import Marvel.Card
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Text.Show qualified

data Treachery = forall a. IsTreachery a => Treachery a

instance Show Treachery where
  show (Treachery a) = show a

instance Eq Treachery where
  Treachery (a :: a) == Treachery (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance ToJSON Treachery where
  toJSON (Treachery a) = toJSON a

data SomeTreacheryCard = forall a. IsTreachery a => SomeTreacheryCard (TreacheryCard a)

liftTreacheryCard :: (forall a . TreacheryCard a -> b) -> SomeTreacheryCard -> b
liftTreacheryCard f (SomeTreacheryCard a) = f a

someTreacheryCardCode :: SomeTreacheryCard -> CardCode
someTreacheryCardCode = liftTreacheryCard cbCardCode

instance Entity Treachery where
  type EntityId Treachery = TreacheryId
  type EntityAttrs Treachery = TreacheryAttrs
  toId = toId . toAttrs
  toAttrs (Treachery a) = toAttrs a

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

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, Entity a, EntityAttrs a ~ TreacheryAttrs, EntityId a ~ TreacheryId, RunMessage a) => IsTreachery a

type TreacheryCard a = CardBuilder TreacheryId a

data TreacheryAttrs = TreacheryAttrs
  { treacheryId :: TreacheryId
  , treacheryCardDef :: CardDef
  , treacherySurge :: Bool
  , treacheryResolver :: Maybe IdentityId
  -- used when we need to remember for other messages
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

surgeL :: Lens' TreacheryAttrs Bool
surgeL = lens treacherySurge $ \m x -> m { treacherySurge = x }

resolverL :: Lens' TreacheryAttrs (Maybe IdentityId)
resolverL = lens treacheryResolver $ \m x -> m { treacheryResolver = x }

instance HasCardCode TreacheryAttrs where
  toCardCode = toCardCode . treacheryCardDef

treacheryWith :: (TreacheryAttrs -> a) -> CardDef -> (TreacheryAttrs -> TreacheryAttrs) -> CardBuilder TreacheryId a
treacheryWith f cardDef g = treachery (f . g) cardDef

treachery :: (TreacheryAttrs -> a) -> CardDef -> CardBuilder TreacheryId a
treachery f cardDef = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \mid -> f $ TreacheryAttrs
    { treacheryId = mid
    , treacheryCardDef = cardDef
    , treacherySurge = False
    , treacheryResolver = Nothing
    }
  }

instance Entity TreacheryAttrs where
  type EntityId TreacheryAttrs = TreacheryId
  type EntityAttrs TreacheryAttrs = TreacheryAttrs
  toId = treacheryId
  toAttrs = id

instance IsSource TreacheryAttrs where
  toSource = TreacherySource . toId

instance IsTarget TreacheryAttrs where
  toTarget = TreacheryTarget . toId

instance IsCard TreacheryAttrs where
  toCard a = EncounterCard $ MkEncounterCard
    { ecCardId = CardId $ unTreacheryId $ toId a
    , ecCardDef = getCardDef a
    }

instance HasCardDef TreacheryAttrs where
  getCardDef = treacheryCardDef

instance RunMessage TreacheryAttrs where
  runMessage msg attrs = case msg of
    TreacheryMessage tid msg' | tid == toId attrs -> case msg' of
      ResolvedTreachery identityId -> do
        pushAll
          $ RemoveFromPlay (toTarget attrs)
          : [ Surge identityId | treacherySurge attrs ]
        pure attrs
      _ -> pure attrs
    _ -> pure attrs
