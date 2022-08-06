module Marvel.Treachery where

import Marvel.Prelude

import Data.Typeable
import Marvel.Card
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Attrs
import Marvel.Treachery.Treacheries
import Text.Show qualified

data Treachery = forall a. IsTreachery a => Treachery a

instance Show Treachery where
  show (Treachery a) = show a

instance Eq Treachery where
  (Treachery (a :: a)) == (Treachery (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance ToJSON Treachery where
  toJSON (Treachery a) = toJSON a

instance FromJSON Treachery where
  parseJSON v = flip (withObject "Treachery") v $ \o -> do
    cardDef <- o .: "treacheryCardDef"
    withTreacheryCardCode (cdCardCode cardDef) $ \(_ :: TreacheryCard a) -> Treachery <$> parseJSON @a v

withTreacheryCardCode
  :: CardCode
  -> (forall a. IsTreachery a => TreacheryCard a -> r)
  -> r
withTreacheryCardCode cCode f =
  case lookup cCode allTreacheries of
    Nothing -> error "invalid treachery"
    Just (SomeTreacheryCard a) -> f a

data SomeTreacheryCard = forall a. IsTreachery a => SomeTreacheryCard (TreacheryCard a)

liftTreacheryCard :: (forall a . TreacheryCard a -> b) -> SomeTreacheryCard -> b
liftTreacheryCard f (SomeTreacheryCard a) = f a

someTreacheryCardCode :: SomeTreacheryCard -> CardCode
someTreacheryCardCode = liftTreacheryCard cbCardCode

allTreacheries :: HashMap CardCode SomeTreacheryCard
allTreacheries = fromList $ map
  (toFst someTreacheryCardCode)
  [ SomeTreacheryCard hardToKeepDown
  , SomeTreacheryCard imTough
  , SomeTreacheryCard stampede
  , SomeTreacheryCard explosion
  , SomeTreacheryCard falseAlarm
  , SomeTreacheryCard klawsVengeance
  , SomeTreacheryCard sonicBoom
  , SomeTreacheryCard soundManipulation
  , SomeTreacheryCard mastersOfMayhem
  , SomeTreacheryCard heartShapedHerb
  , SomeTreacheryCard ritualCombat
  , SomeTreacheryCard titaniasFury
  , SomeTreacheryCard sweepingSwoop
  , SomeTreacheryCard theVulturesPlans
  , SomeTreacheryCard electricWhipAttack
  , SomeTreacheryCard electromagneticBacklash
  , SomeTreacheryCard advance
  , SomeTreacheryCard assault
  , SomeTreacheryCard caughtOffGuard
  , SomeTreacheryCard gangUp
  , SomeTreacheryCard shadowOfThePast
  ]

lookupTreachery :: CardCode -> TreacheryId -> Treachery
lookupTreachery cardCode = case lookup cardCode allTreacheries of
  Just (SomeTreacheryCard a) -> Treachery <$> cbCardBuilder a
  Nothing -> error $ "Invalid card code for treachery " <> show cardCode

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
