module Marvel.Treachery.Attrs where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Queue
import Marvel.Source
import Marvel.Target

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
