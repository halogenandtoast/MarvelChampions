{-# LANGUAGE DuplicateRecordFields #-}

-- | Decks: importing somebody else's deck list, and keeping it.
--
-- A deck comes from a MarvelCDB link or the contents of a deck JSON file in the shape
-- MarvelCDB hands out. The link is the only thing the server ever fetches for a user,
-- so it is restricted to MarvelCDB by host: see 'marvelcdbId'.
module Decks (
  ImportRequest,
  savesDeck,
  importDeck,
  previewDeck,
  describeImportError,
  saveDeck,
  listDecks,
  loadDeck,
  deckListFor,
  marvelcdbId,
  MarvelcdbDeckKind (..),
) where

import AppPrelude
import CardDb
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Char (isDigit)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Database.Persist
import Database.Persist.Sql (SqlBackend, fromSqlKey)
import Marvel.Card.Code (CardCode (..))
import Marvel.Decklist (DeckList (..), aspectsBalanced, missingCards)
import Marvel.I18n (ikey, ikeyWith, textVar)
import Model
import Network.HTTP.Client (Manager, httpLbs, parseRequest, requestHeaders, responseBody, responseStatus)
import Network.HTTP.Types (statusCode)

-- | What the deck page posts: a MarvelCDB link or a deck file's contents, and optionally
-- a name to use instead of the source's. Saved unless @save@ is false, as for a deck
-- loaded only to play the game at hand.
data ImportRequest = ImportRequest {url :: Maybe Text, deck :: Maybe Value, name :: Maybe Text, save :: Maybe Bool}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

savesDeck :: ImportRequest -> Bool
savesDeck request = fromMaybe True request.save

data ImportError
  = NothingToImport
  | NotAMarvelcdbUrl Text
  | FetchFailed Text
  | MalformedJson String
  | NoHero

describeImportError :: ImportError -> Text
describeImportError = \case
  NothingToImport -> ikey "error.noDeckSource"
  NotAMarvelcdbUrl u -> ikeyWith "error.notMarvelcdbLink" [textVar "url" u]
  FetchFailed why -> ikeyWith "error.fetchFailed" [textVar "reason" why]
  MalformedJson why -> ikeyWith "error.malformedDeck" [textVar "reason" (pack why)]
  NoHero -> ikey "error.noHero"

-- | A MarvelCDB deck list. Its aspect is JSON inside a string, under @meta@.
data MarvelcdbDeck = MarvelcdbDeck
  { marvelcdbDeckId :: Maybe Int
  , name :: Text
  , description :: Text
  , hero :: Maybe Text
  , aspects :: [Text]
  , slots :: Map Text Int
  }

instance FromJSON MarvelcdbDeck where
  parseJSON = withObject "MarvelcdbDeck" \o -> do
    marvelcdbDeckId <- o .:? "id"
    name <- o .:? "name" .!= "Imported deck"
    description <- fromMaybe "" <$> o .:? "description_md"
    hero <- o .:? "hero_code" <|> o .:? "investigator_code"
    meta <- o .:? "meta"
    let aspects = case meta of
          Just (Aeson.String t) | Just (Aeson.Object m) <- Aeson.decodeStrict (encodeUtf8 t) -> aspectsIn m
          Just (Aeson.Object m) -> aspectsIn m
          _ -> []
    slots <- codeMap o "slots"
    pure MarvelcdbDeck {..}
   where
    aspectsIn m = [a | k <- ["aspect", "aspect2"], Just (Aeson.String a) <- [KeyMap.lookup k m]]

-- | A @{code: quantity}@ object, tolerating a missing key, an empty array and quantities
-- written as strings.
codeMap :: Aeson.Object -> Key.Key -> Parser (Map Text Int)
codeMap o key = case KeyMap.lookup key o of
  Nothing -> pure mempty
  Just Aeson.Null -> pure mempty
  Just (Aeson.Array _) -> pure mempty
  Just (Aeson.Object entries) ->
    Map.fromList . filter ((> 0) . snd)
      <$> traverse (\(k, v) -> (Key.toText k,) <$> quantity v) (KeyMap.toList entries)
  Just _ -> fail (show key <> " should be an object of card code to quantity")
 where
  quantity = \case
    Aeson.Number n -> pure (truncate n)
    Aeson.String s | Just n <- readMay (unpack s) -> pure n
    v -> fail ("card quantity should be a number, got " <> show v)

-- | One line of a deck list, as the deck page reads it.
data Slot = Slot {quantity :: Int, code :: Text, card :: Maybe Value}
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data HeroSummary = HeroSummary {code :: Text, name :: Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data Deck = Deck
  { deckId :: Int64
  , name :: Text
  , description :: Text
  , hero :: HeroSummary
  , aspects :: [Text]
  , source :: Text
  , sourceId :: Maybe Text
  , sourceUrl :: Maybe Text
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , slots :: [Slot]
  , problem :: Maybe Text
  -- ^ why the engine cannot deal it, if it cannot
  }
  deriving stock (Generic)

instance ToJSON Deck where
  toJSON = genericToJSON idField

data DeckSummary = DeckSummary
  { deckId :: Int64
  , name :: Text
  , hero :: HeroSummary
  , aspects :: [Text]
  , source :: Text
  , sourceUrl :: Maybe Text
  , cardCount :: Int
  , problem :: Maybe Text
  , updatedAt :: UTCTime
  , usedAt :: Maybe UTCTime
  }
  deriving stock (Generic)

instance ToJSON DeckSummary where
  toJSON = genericToJSON idField

idField :: Options
idField = defaultOptions {fieldLabelModifier = \case "deckId" -> "id"; field -> field}

-- | A deck list read from its source, owned by nobody yet.
data ParsedDeck = ParsedDeck
  { name :: Text
  , description :: Text
  , hero :: Text
  , aspects :: [Text]
  , source :: Text
  , sourceId :: Maybe Text
  , sourceUrl :: Maybe Text
  , slots :: [(Text, Int)]
  }
  deriving stock (Generic)

-- | A deck loaded but not kept: what it holds, and the list the engine would deal from
-- it, or why it cannot.
data DeckPreview = DeckPreview
  { name :: Text
  , hero :: HeroSummary
  , aspects :: [Text]
  , slots :: [Slot]
  , cardCount :: Int
  , list :: Maybe DeckList
  , problem :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

previewDeck :: CardDb -> ParsedDeck -> DeckPreview
previewDeck db parsed =
  DeckPreview
    { name = parsed.name
    , hero = heroSummary db parsed.hero
    , aspects = parsed.aspects
    , slots = [slot db c n | (c, n) <- parsed.slots]
    , cardCount = drawCount db parsed.slots
    , list = either (const Nothing) Just dealt
    , problem = either Just (const Nothing) dealt
    }
 where
  dealt = listFrom db parsed.hero parsed.aspects parsed.slots

heroSummary :: CardDb -> Text -> HeroSummary
heroSummary db code = HeroSummary code (fromMaybe code (cardName code db))

-- | The cards a deck is shuffled from: everything but the identity it is built around.
drawCount :: CardDb -> [(Text, Int)] -> Int
drawCount db slots = sum [n | (c, n) <- slots, not (isIdentity c db)]

slot :: CardDb -> Text -> Int -> Slot
slot db code quantity = Slot {quantity, code, card = lookupCard code db}

-- | MarvelCDB has two kinds of deck: a published decklist, public to all, and a deck in
-- somebody's collection, readable when its owner has made their decks public.
data MarvelcdbDeckKind = Decklist | SharedDeck
  deriving stock (Show, Eq)

-- | The kind of deck and its id in a MarvelCDB URL, or 'Nothing' for anything else. A
-- bare number is taken for a published decklist.
--
-- Only @marvelcdb.com@ and @www.marvelcdb.com@, and only a path naming a deck. The
-- server never follows the URL it was given: it takes an id out of it and builds its
-- own, so a link pointed elsewhere on the network goes nowhere.
marvelcdbId :: Text -> Maybe (MarvelcdbDeckKind, Text)
marvelcdbId raw
  | T.null stripped = Nothing
  | allDigits stripped = Just (Decklist, stripped)
  | not onMarvelcdb = Nothing
  | otherwise = case break (`elem` deckPaths) segments of
      (_, path : rest) -> (if path `elem` ["deck", "decks"] then SharedDeck else Decklist,) <$> firstNumber rest
      _ -> Nothing
 where
  stripped = T.strip raw
  withoutScheme = snd (T.breakOnEnd "://" stripped)
  (hostPort, afterHost) = T.break (== '/') withoutScheme
  host = T.toLower (T.takeWhile (/= ':') (snd (T.breakOnEnd "@" hostPort)))
  onMarvelcdb = host `elem` ["marvelcdb.com", "www.marvelcdb.com"]
  segments = T.splitOn "/" (T.dropWhile (== '/') (T.takeWhile (`notElem` ['?', '#']) afterHost))
  deckPaths = ["decklist", "deck", "decklists", "decks"]
  firstNumber = \case
    [] -> Nothing
    p : rest | allDigits p -> Just p | otherwise -> firstNumber rest
  allDigits t = not (T.null t) && T.all isDigit t

-- | Where a deck of each kind is read and where people look at it.
marvelcdbApi, marvelcdbPage :: MarvelcdbDeckKind -> Text -> Text
marvelcdbApi kind ident = case kind of
  Decklist -> "https://marvelcdb.com/api/public/decklist/" <> ident
  SharedDeck -> "https://marvelcdb.com/api/public/deck/" <> ident
marvelcdbPage kind ident = case kind of
  Decklist -> "https://marvelcdb.com/decklist/view/" <> ident
  SharedDeck -> "https://marvelcdb.com/deck/view/" <> ident

-- | Fetch a deck by kind and id. The URL is built here, never taken from the user.
fetchMarvelcdbDeck :: Manager -> MarvelcdbDeckKind -> Text -> IO (Either ImportError Value)
fetchMarvelcdbDeck manager kind ident = do
  attempt <- tryAny do
    request <- parseRequest (unpack (marvelcdbApi kind ident))
    httpLbs request {requestHeaders = [("accept", "application/json"), ("user-agent", "marvel-champions")]} manager
  pure case attempt of
    Left err -> Left (FetchFailed (pack (takeWhile (/= '\n') (show err))))
    Right response -> case statusCode (responseStatus response) of
      404 -> Left (FetchFailed ("MarvelCDB has no public deck " <> ident))
      status
        | status >= 400 -> Left (FetchFailed ("MarvelCDB answered " <> tshow status))
        | otherwise -> case eitherDecode (responseBody response) of
            -- MarvelCDB answers an unknown or private deck with an HTML page and a 200
            Left _ -> Left (FetchFailed ("MarvelCDB did not return a deck for " <> ident))
            Right value -> Right value

-- | The whole import: fetch if asked to, parse.
importDeck :: Manager -> ImportRequest -> IO (Either ImportError ParsedDeck)
importDeck manager request = case (T.strip <$> request.url, request.deck) of
  (Just u, _) | not (T.null u) -> case marvelcdbId u of
    Nothing -> pure (Left (NotAMarvelcdbUrl u))
    Just (kind, ident) ->
      (>>= finish "marvelcdb" (Just (marvelcdbPage kind ident))) <$> fetchMarvelcdbDeck manager kind ident
  (_, Just value) -> pure (finish "json" Nothing value)
  _ -> pure (Left NothingToImport)
 where
  finish source fallback value = case fromJSON value of
    Aeson.Error why -> Left (MalformedJson why)
    Aeson.Success (deck :: MarvelcdbDeck) -> case deck.hero of
      Nothing -> Left NoHero
      Just hero ->
        Right
          ParsedDeck
            { name = case T.strip <$> request.name of
                Just n | not (T.null n) -> n
                _ -> T.strip deck.name
            , description = deck.description
            , hero
            , aspects = deck.aspects
            , source
            , sourceId = tshow <$> deck.marvelcdbDeckId
            , sourceUrl = fallback
            , slots = Map.toAscList deck.slots
            }

-- | Write a parsed deck to a user's collection and return it as stored.
saveDeck :: (MonadIO m) => CardDb -> UserId -> ParsedDeck -> ReaderT SqlBackend m Deck
saveDeck db userId parsed = do
  now <- liftIO getCurrentTime
  let row =
        MarvelDeck
          { marvelDeckUserId = userId
          , marvelDeckName = parsed.name
          , marvelDeckDescription = parsed.description
          , marvelDeckHero = parsed.hero
          , marvelDeckAspects = parsed.aspects
          , marvelDeckSource = parsed.source
          , marvelDeckSourceId = parsed.sourceId
          , marvelDeckSourceUrl = parsed.sourceUrl
          , marvelDeckCreatedAt = now
          , marvelDeckUpdatedAt = now
          , marvelDeckUsedAt = Nothing
          }
  key <- insert row
  insertMany_ [MarvelDeckSlot key position c n | (position, (c, n)) <- zip [0 ..] parsed.slots]
  pure $ toDeck db key row parsed.slots

toDeck :: CardDb -> MarvelDeckId -> MarvelDeck -> [(Text, Int)] -> Deck
toDeck db key d slots =
  Deck
    { deckId = fromSqlKey key
    , name = d.marvelDeckName
    , description = d.marvelDeckDescription
    , hero = heroSummary db d.marvelDeckHero
    , aspects = d.marvelDeckAspects
    , source = d.marvelDeckSource
    , sourceId = d.marvelDeckSourceId
    , sourceUrl = d.marvelDeckSourceUrl
    , createdAt = d.marvelDeckCreatedAt
    , updatedAt = d.marvelDeckUpdatedAt
    , slots = [slot db c n | (c, n) <- slots]
    , problem = either Just (const Nothing) (listFrom db d.marvelDeckHero d.marvelDeckAspects slots)
    }

slotsOf :: (MonadIO m) => MarvelDeckId -> ReaderT SqlBackend m [(Text, Int)]
slotsOf key = map ((\s -> (s.marvelDeckSlotCardCode, s.marvelDeckSlotQuantity)) . entityVal) <$> selectList [MarvelDeckSlotDeckId ==. key] [Asc MarvelDeckSlotPosition]

-- | A user's decks, most recently touched first.
listDecks :: (MonadIO m) => CardDb -> UserId -> ReaderT SqlBackend m [DeckSummary]
listDecks db userId = do
  decks <- selectList [MarvelDeckUserId ==. userId] [Desc MarvelDeckUpdatedAt, Desc MarvelDeckId]
  for decks \(Entity key d) -> do
    slots <- slotsOf key
    pure
      DeckSummary
        { deckId = fromSqlKey key
        , name = d.marvelDeckName
        , hero = heroSummary db d.marvelDeckHero
        , aspects = d.marvelDeckAspects
        , source = d.marvelDeckSource
        , sourceUrl = d.marvelDeckSourceUrl
        , cardCount = drawCount db slots
        , problem = either Just (const Nothing) (listFrom db d.marvelDeckHero d.marvelDeckAspects slots)
        , updatedAt = d.marvelDeckUpdatedAt
        , usedAt = d.marvelDeckUsedAt
        }

-- | One deck with its cards, or 'Nothing' for a deck this user does not own.
loadDeck :: (MonadIO m) => CardDb -> UserId -> MarvelDeckId -> ReaderT SqlBackend m (Maybe Deck)
loadDeck db userId key =
  get key >>= \case
    Just d | d.marvelDeckUserId == userId -> Just . toDeck db key d <$> slotsOf key
    _ -> pure Nothing

-- | A stored deck as the engine deals it, refused, with the cards named, when the engine
-- does not know a card.
deckListFor :: (MonadIO m) => CardDb -> UserId -> MarvelDeckId -> ReaderT SqlBackend m (Either Text DeckList)
deckListFor db userId key =
  get key >>= \case
    Just d | d.marvelDeckUserId == userId -> listFrom db d.marvelDeckHero d.marvelDeckAspects <$> slotsOf key
    _ -> pure $ Left $ ikey "error.noSuchDeck"

-- | The deck as the engine deals it: the identity out of the cards, the rest as they are.
listFrom :: CardDb -> Text -> [Text] -> [(Text, Int)] -> Either Text DeckList
listFrom db hero aspects slots = case missingCards deckList of
  [] | not (aspectsBalanced deckList) -> Left $ ikey "error.unbalancedAspects"
  [] -> Right deckList
  missing -> Left $ ikeyWith "error.unknownCards" [textVar "cards" (intercalate ", " [fromMaybe c (cardName c db) | CardCode c <- missing])]
 where
  deckList = DeckList (CardCode hero) aspects [(CardCode c, n) | (c, n) <- slots, not (isIdentity c db)]
