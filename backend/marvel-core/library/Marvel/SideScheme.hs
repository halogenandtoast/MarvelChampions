module Marvel.SideScheme where

import Marvel.Prelude

import Data.Typeable
import Marvel.Card
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Modifier
import Marvel.SideScheme.Attrs
import Marvel.SideScheme.SideSchemes
import Marvel.Source
import Marvel.Target
import Text.Show qualified

data SideScheme = forall a. IsSideScheme a => SideScheme a

instance ToJSON SideScheme where
  toJSON (SideScheme a) = toJSON a

instance Show SideScheme where
  show (SideScheme a) = show a

instance Eq SideScheme where
  (SideScheme (a :: a)) == (SideScheme (b :: b)) =
    case eqT @a @b of
      Just Refl -> a == b
      Nothing -> False

instance FromJSON SideScheme where
  parseJSON v = flip (withObject "SideScheme") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    withSideSchemeCardCode cCode $ \(_ :: SideSchemeCard a) -> SideScheme <$> parseJSON @a v

withSideSchemeCardCode
  :: CardCode
  -> (forall a. IsSideScheme a => SideSchemeCard a -> r)
  -> r
withSideSchemeCardCode cCode f =
  case lookup cCode allSideSchemes of
    Nothing -> error "invalid side scheme"
    Just (SomeSideSchemeCard a) -> f a

data SomeSideSchemeCard = forall a. IsSideScheme a => SomeSideSchemeCard (SideSchemeCard a)

liftSideSchemeCard :: (forall a . SideSchemeCard a -> b) -> SomeSideSchemeCard -> b
liftSideSchemeCard f (SomeSideSchemeCard a) = f a

someSideSchemeCardCode :: SomeSideSchemeCard -> CardCode
someSideSchemeCardCode = liftSideSchemeCard cbCardCode

allSideSchemes :: HashMap CardCode SomeSideSchemeCard
allSideSchemes =
  fromList $
    map
      (toFst someSideSchemeCardCode)
      [ SomeSideSchemeCard breakinAndTakin
      , SomeSideSchemeCard crowdControl
      , SomeSideSchemeCard bombScare
      , SomeSideSchemeCard defenseNetwork
      , SomeSideSchemeCard illegalArmsFactory
      , SomeSideSchemeCard theImmortalKlaw
      , SomeSideSchemeCard theMastersOfEvil
      , SomeSideSchemeCard usurpTheThrone
      , SomeSideSchemeCard personalChallenge
      , SomeSideSchemeCard highwayRobbery
      , SomeSideSchemeCard imminentOverload
      ]


lookupSideScheme :: CardCode -> SideSchemeId -> SideScheme
lookupSideScheme cardCode = case lookup cardCode allSideSchemes of
  Just (SomeSideSchemeCard a) -> SideScheme <$> cbCardBuilder a
  Nothing -> error $ "Invalid card code for side scheme " <> show cardCode

instance Entity SideScheme where
  type EntityId SideScheme = SideSchemeId
  type EntityAttrs SideScheme = SideSchemeAttrs
  toId = toId . toAttrs
  toAttrs (SideScheme a) = toAttrs a

instance RunMessage SideScheme where
  runMessage msg (SideScheme a) = SideScheme <$> runMessage msg a

instance HasModifiersFor SideScheme where
  getModifiersFor source target (SideScheme a) = getModifiersFor source target a

instance IsSource SideScheme where
  toSource = SideSchemeSource . toId

instance IsTarget SideScheme where
  toTarget = SideSchemeTarget . toId

instance IsCard SideScheme where
  toCard = toCard . toAttrs

instance HasCardDef SideScheme where
  getCardDef = getCardDef . toAttrs

isCrisis :: SideScheme -> Bool
isCrisis = sideSchemeCrisis . toAttrs

getSideSchemeThreat :: SideScheme -> Natural
getSideSchemeThreat = sideSchemeThreat . toAttrs
