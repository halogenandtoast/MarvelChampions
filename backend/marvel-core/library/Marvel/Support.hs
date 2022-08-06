module Marvel.Support where

import Marvel.Prelude

import Data.Typeable
import Marvel.Ability
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Modifier
import Marvel.Source
import Marvel.Support.Attrs
import Marvel.Support.Supports
import Text.Show qualified

data Support = forall a. IsSupport a => Support a

instance Show Support where
  show (Support a) = show a

instance ToJSON Support where
  toJSON (Support a) = toJSON a

instance Eq Support where
  (Support (a :: a)) == (Support (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance FromJSON Support where
  parseJSON v = flip (withObject "Support") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    withSupportCardCode cCode $ \(_ :: SupportCard a) -> Support <$> parseJSON @a v

withSupportCardCode
  :: CardCode
  -> (forall a. IsSupport a => SupportCard a -> r)
  -> r
withSupportCardCode cCode f =
  case lookup cCode allSupports of
    Nothing -> error "invalid support"
    Just (SomeSupportCard a) -> f a

data SomeSupportCard = forall a. IsSupport a => SomeSupportCard (SupportCard a)

liftSupportCard :: (forall a . SupportCard a -> b) -> SomeSupportCard -> b
liftSupportCard f (SomeSupportCard a) = f a

someSupportCardCode :: SomeSupportCard -> CardCode
someSupportCardCode = liftSupportCard cbCardCode

allSupports :: HashMap CardCode SomeSupportCard
allSupports =
  fromList $
    map
      (toFst someSupportCardCode)
      [ SomeSupportCard auntMay
      -- , SomeSupportCard alphaFlightStation
      , SomeSupportCard superhumanLawDivision
      , SomeSupportCard pepperPotts
      , SomeSupportCard starkTower
      , SomeSupportCard theGoldenCity
      , SomeSupportCard tacTeam
      , SomeSupportCard interrogationRoom
      , SomeSupportCard surveillanceTeam
      , SomeSupportCard theTriskellion
      , SomeSupportCard medTeam
      , SomeSupportCard avengersMansion
      , SomeSupportCard helicarrier
      ]

lookupSupport :: CardCode -> IdentityId -> SupportId -> Support
lookupSupport cardCode identityId supportId= case lookup cardCode allSupports of
  Just (SomeSupportCard a) -> Support $ cbCardBuilder a (identityId, supportId)
  Nothing -> error $ "Invalid card code for support " <> show cardCode

instance Entity Support where
  type EntityId Support = SupportId
  type EntityAttrs Support = SupportAttrs
  toId = toId . toAttrs
  toAttrs (Support a) = toAttrs a

instance RunMessage Support where
  runMessage msg (Support a) = Support <$> runMessage msg a

instance Exhaustable Support where
  isExhausted = supportExhausted . toAttrs

instance IsSource Support where
  toSource = SupportSource . toId

instance HasAbilities Support where
  getAbilities (Support a) = getAbilities a

instance HasModifiersFor Support where
  getModifiersFor source target (Support a) = getModifiersFor source target a

getSupportController :: Support -> IdentityId
getSupportController = supportController . toAttrs

getSupportUses :: Support -> Natural
getSupportUses = supportUses . toAttrs
