-- | The card browser's database (@cards.json@), read once at startup so decks can be
-- resolved against it. The cards are kept as the JSON they arrived as, because the
-- deck page renders them with the same code the browser does.
module CardDb (CardDb, loadCardDb, lookupCard, cardName, cardType, cardFaction, isIdentity) where

import AppPrelude
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Map.Strict qualified as Map

data CardInfo = CardInfo
  { code :: Text
  , name :: Text
  , typeCode :: Text
  , faction :: Maybe Text
  }

newtype CardDb = CardDb {byCode :: Map Text (CardInfo, Value)}

loadCardDb :: FilePath -> IO (Either String CardDb)
loadCardDb path = do
  decoded <- eitherDecodeFileStrict' path
  pure do
    cards <- maybe (Left "cards.json has no cards") Right . parseMaybe (withObject "index" (.: "cards")) =<< decoded
    pure $ CardDb $ Map.fromList [(info.code, (info, card)) | card <- cards, Just info <- [parseMaybe cardInfo card]]
 where
  cardInfo :: Value -> Parser CardInfo
  cardInfo = withObject "card" \o -> CardInfo <$> o .: "code" <*> o .: "name" <*> o .: "type" <*> o .:? "faction"

lookupCard :: Text -> CardDb -> Maybe Value
lookupCard code db = snd <$> Map.lookup code db.byCode

cardName :: Text -> CardDb -> Maybe Text
cardName code db = (.name) . fst <$> Map.lookup code db.byCode

cardType :: Text -> CardDb -> Maybe Text
cardType code db = (.typeCode) . fst <$> Map.lookup code db.byCode

cardFaction :: Text -> CardDb -> Maybe Text
cardFaction code db = (.faction) . fst =<< Map.lookup code db.byCode

-- | A hero or alter-ego face: what a deck is built around.
isIdentity :: Text -> CardDb -> Bool
isIdentity code db = cardType code db `elem` [Just "hero", Just "alter_ego"]
