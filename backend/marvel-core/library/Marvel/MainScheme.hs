module Marvel.MainScheme where

import Marvel.Prelude

import Data.Typeable
import Marvel.Card
import Marvel.Entity
import Marvel.Id
import Marvel.MainScheme.Attrs
import Marvel.MainScheme.MainSchemes
import Marvel.Message
import Marvel.Source
import Text.Show qualified

data MainScheme = forall a. IsMainScheme a => MainScheme a

instance Show MainScheme where
  show (MainScheme a) = show a

instance ToJSON MainScheme where
  toJSON (MainScheme a) = toJSON a

instance Eq MainScheme where
  (MainScheme (a :: a)) == (MainScheme (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance FromJSON MainScheme where
  parseJSON v = flip (withObject "MainScheme") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    withMainSchemeCardCode cCode $ \(_ :: MainSchemeCard a) -> MainScheme <$> parseJSON @a v

withMainSchemeCardCode
  :: CardCode
  -> (forall a. IsMainScheme a => MainSchemeCard a -> r)
  -> r
withMainSchemeCardCode cCode f =
  case lookup cCode allMainSchemes of
    Nothing -> error "invalid minion"
    Just (SomeMainSchemeCard a) -> f a

data SomeMainSchemeCard = forall a. IsMainScheme a => SomeMainSchemeCard (MainSchemeCard a)

liftMainSchemeCard :: (forall a . MainSchemeCard a -> b) -> SomeMainSchemeCard -> b
liftMainSchemeCard f (SomeMainSchemeCard a) = f a

someMainSchemeCardCode :: SomeMainSchemeCard -> CardCode
someMainSchemeCardCode = liftMainSchemeCard cbCardCode

allMainSchemes :: HashMap CardCode SomeMainSchemeCard
allMainSchemes =
  fromList $
    map
      (toFst someMainSchemeCardCode)
      [ SomeMainSchemeCard theBreakIn
      , SomeMainSchemeCard undergroundDistribution
      , SomeMainSchemeCard secretRendezvous
      -- , SomeMainSchemeCard theCrimsonCowl
      -- , SomeMainSchemeCard assaultOnNorad
      -- , SomeMainSchemeCard countdownToOblivion
      ]

lookupMainScheme :: CardCode -> MainSchemeId -> Maybe MainScheme
lookupMainScheme cardCode mainSchemeId = case lookup cardCode allMainSchemes of
  Just (SomeMainSchemeCard a) -> Just . MainScheme $ cbCardBuilder a mainSchemeId
  Nothing -> Nothing

instance Entity MainScheme where
  type EntityId MainScheme = MainSchemeId
  type EntityAttrs MainScheme = MainSchemeAttrs
  toId = toId . toAttrs
  toAttrs (MainScheme a) = toAttrs a

instance RunMessage MainScheme where
  runMessage msg (MainScheme a) = MainScheme <$> runMessage msg a

instance IsSource MainScheme where
  toSource = MainSchemeSource . toId

instance IsCard MainScheme where
  toCard = toCard . toAttrs

instance HasCardDef MainScheme where
  getCardDef = getCardDef . toAttrs

getMainSchemeThreat :: MainScheme -> Natural
getMainSchemeThreat = mainSchemeThreat . toAttrs
