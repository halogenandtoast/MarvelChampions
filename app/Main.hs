{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import AppPrelude
import Auth
import BugUpload (uploadBugExport)
import CardDb (CardDb, loadCardDb)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (mfilter)
import Control.Monad.Random (evalRand, mkStdGen, randomIO, randomRIO)
import Data.Aeson (Result (..))
import Data.Aeson.Diff qualified as Diff
import Data.Char (isAlphaNum)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock (addUTCTime, nominalDay)
import Database.Persist.Postgresql
import Decks
import GameLog
import Marvel.Campaign
import Marvel.Card.Code (CardCode (..), toHeroCardCode)
import Marvel.Difficulty (Difficulty (..))
import Marvel.Card.Def (cdName)
import Marvel.Card.Def qualified as CardDef
import Marvel.Decklist
import Marvel.EncounterSet (modularSets)
import Marvel.Entity (toAttrs)
import Marvel.Frontend (FrontendMessage (..))
import Marvel.Game
import Marvel.Game.Public (publicGameFor)
import Marvel.Game.Run (answerQuestion, runDebug, runGame)
import Marvel.I18n (ikey, ikeyWith, textVar)
import Marvel.Id (IdentityId)
import Marvel.Identity (identityDamage, setName)
import Marvel.Identity.Types (Attrs (..))
import Marvel.Log (chat)
import Marvel.Log.Entry
import Marvel.Message (FinishedStatus (..), Message (StartGame))
import Marvel.Name (display)
import Marvel.Phase
import Marvel.Scenario (lookupScenario, recommendedSetup, scenarioCodes)
import Marvel.Scenario.Types (ScenarioSetup (..))
import Marvel.Hero.Cards (allHeroesMap)
import Marvel.Villain.Cards (allVillainsMap)
import Marvel.MainScheme.Cards qualified as MainSchemes
import Marvel.Card.Code (toBaseCardCode)
import Model
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (status400, status401, status409, status500)
import System.Environment (lookupEnv)
import System.Random (StdGen)
import System.Random.Internal (StdGen (..))
import System.Random.SplitMix (seedSMGen', unseedSMGen)
import Text.Read (readMaybe)
import Yesod.Core
import Yesod.Persist.Core
import Yesod.WebSockets

-- | Who plays: with friends, who sits in which seat (seat n playing the nth player);
-- otherwise the owner plays every seat.
data Table = Table
  { withFriends :: Bool
  , seats :: Int
  , owner :: UserId
  , seated :: [SeatView]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- | A seat taken: by whom, and as which hero, so the lobby can show who is coming.
data SeatView = SeatView {position :: Int, userId :: UserId, username :: Text, hero :: Maybe CardCode}
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- | What goes out to a game's clients: what the game said as it is, log entries to the
-- viewers who may read them (by the table and its seats), and the game, sent to each
-- client as its viewer may see it, with the log afresh when it was undone or dealt.
data Outgoing = Said LByteString | Told Table [IdentityId] LogEntry | Updated MarvelGame Table (Maybe Game) (Maybe [LogEntry])

data App = App
  { pool :: ConnectionPool
  , jwtSecret :: Text
  , channels :: TVar (Map MarvelGameId (TChan Outgoing))
  , cardDb :: CardDb
  , manager :: Manager
  }

mkYesod
  "App"
  [parseRoutes|
/api ApiP:
  /games GamesR GET POST
  /games/#MarvelGameId GameR GET POST DELETE
  /games/#MarvelGameId/spectate SpectateR GET
  /games/#MarvelGameId/join JoinR POST
  /games/#MarvelGameId/leave LeaveR POST
  /games/#MarvelGameId/start StartR POST
  /games/#MarvelGameId/undo UndoR POST
  /games/#MarvelGameId/log LogR GET
  /games/#MarvelGameId/chat ChatR POST
  /games/#MarvelGameId/reset ResetR POST
  /games/#MarvelGameId/debug DebugR POST
  /games/#MarvelGameId/export ExportR GET
  /games/#MarvelGameId/file-bug BugR POST
  /spectatable SpectatableR GET
  /admin AdminR GET
  /import ImportR POST
  /scenarios ScenariosR GET
  /campaigns CampaignsR GET POST
  /campaigns/#MarvelCampaignId CampaignR GET DELETE
  /campaigns/#MarvelCampaignId/continue ContinueCampaignR POST
  /campaigns/#MarvelCampaignId/join JoinCampaignR POST
  /campaigns/#MarvelCampaignId/leave LeaveCampaignR POST
  /campaigns/#MarvelCampaignId/start StartCampaignR POST
  /campaigns/#MarvelCampaignId/seats/#Int CampaignSeatR PUT
  /decks DecksR GET POST
  /decks/#MarvelDeckId DeckR GET DELETE
  /starter-decks StarterDecksR GET
  /register RegisterR POST
  /guest GuestR POST
  /authenticate AuthenticateR POST
  /whoami WhoAmIR GET
  /settings SettingsR PUT
|]

-- | The API authenticates with tokens, so there is no cookie session.
instance Yesod App where
  makeSessionBackend _ = pure Nothing

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = getsYesod (.pool) >>= runSqlPool action

data GameData = GameData
  { started :: Bool
  , game :: Maybe Value
  , playerId :: Maybe IdentityId
  , canUndo :: Bool
  , table :: Table
  , setup :: Value
  -- ^ the scenario as it was chosen, for the lobby to show before it is dealt
  , log :: Maybe [LogEntry]
  -- ^ the log's latest entries, when the client should take them afresh
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- | What the home page lists for each game.
data GameSummary = GameSummary
  { id :: MarvelGameId
  , name :: Text
  , playerName :: Text
  , scenario :: Text
  , villain :: CardCode
  , difficulty :: Text
  , round :: Int
  , phase :: Phase
  , outcome :: Maybe FinishedStatus
  , waiting :: Bool
  -- ^ a game with friends that has not been dealt, waiting for seats to fill
  , seats :: [SeatSummary]
  -- ^ who plays, in seat order, and as whom; none until the game is dealt
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data SeatSummary = SeatSummary
  { name :: Text
  , identity :: CardCode
  , damage :: Int
  , eliminated :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- | The signed-in user, if there is one.
lookupUserId :: Handler (Maybe UserId)
lookupUserId = do
  secret <- getsYesod (.jwtSecret)
  subject <- liftIO . fmap join . traverse (verifyToken secret) =<< lookupToken
  pure $ toSqlKey <$> (readMaybe . unpack =<< subject)

-- | The signed-in user, or 401.
requireUserId :: Handler UserId
requireUserId = maybe (sendResponseStatus status401 (ikey "error.signInFirst")) pure =<< lookupUserId

requireAdmin :: Handler ()
requireAdmin = do
  user <- runDB . get404 =<< requireUserId
  unless user.userAdmin $ permissionDenied "admins only"

-- | A game the signed-in user owns, or 404: other people's games do not exist for you.
requireOwnedGame :: MarvelGameId -> Handler MarvelGame
requireOwnedGame gid = do
  userId <- requireUserId
  row <- runDB $ get404 gid
  unless (row.marvelGameUserId == userId) notFound
  pure row

-- | A game you play: your own, or one you have a seat at. A game with friends still
-- waiting for players is open to anyone signed in, so its invite link shows the lobby.
requirePlayableGame :: MarvelGameId -> Handler MarvelGame
requirePlayableGame gid = do
  userId <- requireUserId
  row <- runDB $ get404 gid
  seat <- runDB $ selectFirst [MarvelSeatGameId ==. gid, MarvelSeatUserId ==. userId] []
  unless (row.marvelGameUserId == userId || isJust seat || (row.marvelGameWithFriends && awaitingPlayers row)) notFound
  pure row

-- | A game with friends before it is dealt.
awaitingPlayers :: MarvelGame -> Bool
awaitingPlayers row = row.marvelGameCurrentData == Null

-- | A game you may watch: one open to spectators, your own, or any game for an admin.
requireWatchableGame :: MarvelGameId -> Handler MarvelGame
requireWatchableGame gid = do
  row <- runDB $ get404 gid
  userId <- lookupUserId
  admin <- maybe (pure False) (fmap (maybe False (.userAdmin)) . runDB . get) userId
  seat <- maybe (pure Nothing) (\u -> runDB $ selectFirst [MarvelSeatGameId ==. gid, MarvelSeatUserId ==. u] []) userId
  unless (row.marvelGameAllowSpectators || userId == Just row.marvelGameUserId || isJust seat || admin) notFound
  pure row

data UserView = UserView
  { id :: UserId
  , username :: Text
  , email :: Text
  , admin :: Bool
  , beta :: Bool
  , allowSpectatorsByDefault :: Bool
  , guest :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data Session = Session {token :: Text, user :: UserView}
  deriving stock (Generic)
  deriving anyclass (ToJSON)

newSession :: Entity User -> Handler Value
newSession (Entity userId user) = do
  secret <- getsYesod (.jwtSecret)
  token <- liftIO $ signToken secret (tshow $ fromSqlKey userId)
  pure $ toJSON $ Session token (userView userId user)

userView :: UserId -> User -> UserView
userView userId user = UserView userId user.userUsername user.userEmail user.userAdmin user.userBeta user.userAllowSpectatorsByDefault user.userGuest

data Registration = Registration {username :: Text, email :: Text, password :: Text}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

postRegisterR :: Handler Value
postRegisterR = do
  reg <- requireCheckJsonBody @_ @Registration
  let username = T.strip reg.username
      email = toLower (T.strip reg.email)
  when (null username || null email) $ sendResponseStatus status400 (ikey "error.registrationIncomplete")
  when (length reg.password < 8) $ sendResponseStatus status400 (ikey "error.passwordTooShort")
  -- a guest signing up keeps their games: their row becomes the account
  guest <- runDB . fmap join . traverse (\uid -> fmap (Entity uid) . mfilter (.userGuest) <$> get uid) =<< lookupUserId
  let isOther = maybe False ((/= fmap entityKey guest) . Just . entityKey)
  taken <- runDB $ (||) <$> (isOther <$> getBy (UniqueUsername username)) <*> (isOther <$> getBy (UniqueEmail email))
  when taken $ sendResponseStatus status409 (ikey "error.alreadyRegistered")
  digest <- maybe (error "bcrypt failed") pure =<< liftIO (hashPassword reg.password)
  case guest of
    Just (Entity userId _) ->
      newSession . Entity userId
        =<< runDB (updateGet userId [UserUsername =. username, UserEmail =. email, UserPasswordDigest =. digest, UserGuest =. False])
    Nothing -> do
      now <- liftIO getCurrentTime
      let user = User {userUsername = username, userEmail = email, userPasswordDigest = digest, userAdmin = False, userBeta = False, userAllowSpectatorsByDefault = False, userGuest = False, userCreatedAt = now}
      userId <- runDB $ insert user
      newSession (Entity userId user)

-- | Play without an account: a user with a made-up name and no password.
postGuestR :: Handler Value
postGuestR = do
  now <- liftIO getCurrentTime
  let attempt = do
        n <- liftIO $ randomRIO (100000, 999999 :: Int)
        let username = "Guest" <> tshow n
            user = User {userUsername = username, userEmail = toLower username <> "@guest.invalid", userPasswordDigest = "", userAdmin = False, userBeta = False, userAllowSpectatorsByDefault = False, userGuest = True, userCreatedAt = now}
        runDB (insertUnique user) >>= maybe attempt (newSession . (`Entity` user))
  attempt

data Credentials = Credentials {login :: Text, password :: Text}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

-- | Sign in with either a username or an email address.
postAuthenticateR :: Handler Value
postAuthenticateR = do
  creds <- requireCheckJsonBody @_ @Credentials
  let login = T.strip creds.login
  found <- runDB $ maybe (getBy $ UniqueEmail $ toLower login) (pure . Just) =<< getBy (UniqueUsername login)
  case found of
    Just user | not (entityVal user).userGuest, checkPassword (entityVal user).userPasswordDigest creds.password -> newSession user
    _ -> sendResponseStatus status401 (ikey "error.badLogin")

getWhoAmIR :: Handler Value
getWhoAmIR = do
  userId <- requireUserId
  -- a swept guest's token outlives them: signed out
  user <- maybe (sendResponseStatus status401 (ikey "error.signInFirst")) pure =<< runDB (get userId)
  pure $ toJSON $ userView userId user

newtype Settings = Settings {allowSpectatorsByDefault :: Bool}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

putSettingsR :: Handler Value
putSettingsR = do
  userId <- requireUserId
  settings <- requireCheckJsonBody @_ @Settings
  user <- runDB $ updateGet userId [UserAllowSpectatorsByDefault =. settings.allowSpectatorsByDefault]
  pure $ toJSON $ userView userId user

-- | Every connected client of a game listens on that game's channel.
gameChannel :: MarvelGameId -> Handler (TChan Outgoing)
gameChannel gid = do
  app <- getYesod
  atomically do
    channels <- readTVar app.channels
    case Map.lookup gid channels of
      Just chan -> pure chan
      Nothing -> do
        chan <- newBroadcastTChan
        writeTVar app.channels (Map.insert gid chan channels)
        pure chan

decodeOrFail :: (FromJSON a) => Text -> Value -> Handler a
decodeOrFail what v = case fromJSON v of
  Success a -> pure a
  Error err -> sendResponseStatus status400 ("could not read the stored " <> what <> ": " <> pack err)

-- | A random generator is stored as its splitmix seed, so a reloaded game rolls the same.
showGen :: StdGen -> Text
showGen = tshow . unseedSMGen . unStdGen

readGen :: Text -> Handler StdGen
readGen =
  maybe (sendResponseStatus status400 ("could not read the stored random generator" :: Text)) (pure . StdGen . seedSMGen')
    . readMaybe
    . unpack

-- | Load a stored game into a fresh app, with what it says kept to send once the move
-- is stored.
loadGame :: MarvelGame -> Handler (GameApp, IORef [FrontendMessage])
loadGame row = do
  g <- decodeOrFail "game" row.marvelGameCurrentData
  queue <- decodeOrFail "queue" row.marvelGameCurrentQueue
  gen <- readGen row.marvelGameRandomGen
  liftIO $ appFor g queue gen

-- | What the game says is kept, newest first, rather than sent as it is said: its log
-- entries are numbered as they are stored, which has to wait for the move to be.
appFor :: Game -> [Message] -> StdGen -> IO (GameApp, IORef [FrontendMessage])
appFor g queue gen = do
  said <- newIORef []
  app <- newGameApp g queue gen (\msg -> modifyIORef' said (msg :))
  pure (app, said)

-- | What the game said, in order, to the game's clients: each log entry to whoever may
-- read it.
publish :: MarvelGameId -> MarvelGame -> [IdentityId] -> [FrontendMessage] -> Handler ()
publish gid row seatOrder said = do
  chan <- gameChannel gid
  table <- tableFor gid row
  atomically $ for_ said $ traverse_ (writeTChan chan) . \case
    Logged e -> Just $ Told table seatOrder e
    Board b -> case fromJSON b of
      Success g -> Just $ Updated row table (Just g) Nothing
      Error _ -> Nothing
    msg -> Just $ Said $ encode msg

-- | The game, nothing while it waits for players.
storedGame :: MarvelGame -> Handler (Maybe Game)
storedGame row = if awaitingPlayers row then pure Nothing else Just <$> decodeOrFail "game" row.marvelGameCurrentData

tableFor :: MarvelGameId -> MarvelGame -> Handler Table
tableFor gid row = do
  seated <- runDB do
    seats <- selectList [MarvelSeatGameId ==. gid] [Asc MarvelSeatPosition]
    for seats \(Entity _ seat) -> do
      name <- maybe "" (.userUsername) <$> get seat.marvelSeatUserId
      let hero = case fromJSON <$> seat.marvelSeatDeck of
            Just (Success (d :: DeckList)) -> Just d.hero
            _ -> Nothing
      pure $ SeatView seat.marvelSeatPosition seat.marvelSeatUserId name hero
  pure Table {withFriends = row.marvelGameWithFriends, seats = row.marvelGameSeats, owner = row.marvelGameUserId, seated}

-- | The game as a viewer may see it: with friends, only your own seat's hand, and only
-- the log entries meant for whoever sees it.
gameData :: (MonadIO m) => Maybe UserId -> MarvelGame -> Table -> Maybe Game -> Maybe [LogEntry] -> m GameData
gameData viewer row table g entries = do
  view <- liftIO $ for g \g' -> publicGameFor (seesHand table viewer (gameSeats g')) g'
  pure $ GameData
    (isJust g)
    view
    (gameActivePlayer <$> g)
    (row.marvelGameStep > 0)
    table
    row.marvelGameScenario
    (filter (readsEntry $ seesHand table viewer (maybe [] gameSeats g)) <$> entries)

-- | Whether a viewer sees a player's hand: with friends, only their own seat's.
seesHand :: Table -> Maybe UserId -> [IdentityId] -> IdentityId -> Bool
seesHand table viewer seatOrder pid
  | table.withFriends = isJust viewer && any (\seat -> Just seat.userId == viewer && seatOrder `atMay` seat.position == Just pid) table.seated
  | otherwise = True

updateFor :: (MonadIO m) => Maybe UserId -> MarvelGame -> Table -> Maybe Game -> Maybe [LogEntry] -> m LByteString
updateFor viewer row table g entries = do
  contents <- gameData viewer row table g entries
  pure $ encode $ object ["tag" .= String "Update", "contents" .= contents]

-- | The game to its clients, with the log afresh when given.
broadcast :: MarvelGameId -> MarvelGame -> Maybe Game -> Maybe [LogEntry] -> Handler ()
broadcast gid row g entries = do
  chan <- gameChannel gid
  table <- tableFor gid row
  atomically $ writeTChan chan $ Updated row table g entries

villainOf :: ScenarioSetup -> (Text, CardCode)
-- a scenario of several villains (Breakout) is set up by, and named for, its main scheme
villainOf setup = (maybe (unCardCode setup.scenarioCode) (display . cdName) (HashMap.lookup setup.scenarioCode allVillainsMap <|> HashMap.lookup (toBaseCardCode setup.scenarioCode) MainSchemes.allMainSchemes), setup.scenarioCode)

summary :: Entity MarvelGame -> Maybe GameSummary
summary (Entity gid row) = do
  setup <- case fromJSON row.marvelGameScenario of
    Success s -> Just s
    Error _ -> Nothing
  let (villainName, villainCode) = villainOf setup
      base =
        GameSummary
          { id = gid
          , name = row.marvelGameName
          , playerName = row.marvelGamePlayerName
          , scenario = villainName
          , villain = villainCode
          , difficulty = tshow setup.difficulty
          , round = 0
          , phase = PlayerPhase
          , outcome = Nothing
          , waiting = True
          , seats = []
          }
  if awaitingPlayers row
    then pure base
    else case fromJSON row.marvelGameCurrentData of
      Error _ -> Nothing
      Success (g :: Game) ->
        Just
          base
            { round = gameRound g
            , phase = gamePhase g
            , outcome = case gameState g of
                Finished status -> Just status
                _ -> Nothing
            , waiting = False
            , seats =
                [ case HashMap.lookup pid (gamePlayers g) of
                    Just p -> SeatSummary (toAttrs p).playerIdentityName (CardDef.cdCardCode $ CardDef.getCardDef p) (fromIntegral $ identityDamage p) False
                    Nothing -> SeatSummary "" "" 0 True
                | pid <- gameSeats g
                ]
            }

getGamesR :: Handler Value
getGamesR = do
  userId <- requireUserId
  games <- runDB do
    seatedAt <- map ((.marvelSeatGameId) . entityVal) <$> selectList [MarvelSeatUserId ==. userId] []
    selectList ([MarvelGameUserId ==. userId] ||. [MarvelGameId <-. seatedAt]) [Desc MarvelGameUpdatedAt]
  pure $ toJSON $ mapMaybe summary games

-- | A starter, one of your decks, or a deck loaded for this game alone and not kept.
data DeckChoice = StarterDeck Text | ImportedDeck MarvelDeckId | ListedDeck DeckList

instance FromJSON DeckChoice where
  parseJSON = withObject "DeckChoice" \o -> (StarterDeck <$> o .: "starter") <|> (ImportedDeck <$> o .: "deckId") <|> (ListedDeck <$> o .: "list")

data NewGame = NewGame
  { name :: Text
  , seed :: Maybe Int
  , scenario :: Maybe ScenarioSetup
  , decks :: [DeckChoice]
  -- ^ one deck per seat you take. None leaves every seat open, to take in the lobby
  , seats :: Maybe Int
  -- ^ how many play, at least one per deck; you take the first seats and the rest are
  -- open, the game waiting for them
  , allowSpectators :: Maybe Bool
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

newtype JoinGame = JoinGame {deck :: DeckChoice}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

resolveDeck :: UserId -> DeckChoice -> Handler DeckList
resolveDeck userId = \case
  StarterDeck hero ->
    maybe (sendResponseStatus status400 (ikeyWith "error.noStarterDeck" [textVar "hero" hero])) pure $ lookupStarterDeck hero
  ImportedDeck deckId -> do
    db <- getsYesod (.cardDb)
    deckList <- runDB (deckListFor db userId deckId) >>= either (sendResponseStatus status400) pure
    now <- liftIO getCurrentTime
    runDB $ update deckId [MarvelDeckUsedAt =. Just now]
    pure deckList
  ListedDeck deckList
    | not (aspectsBalanced deckList) -> sendResponseStatus status400 (ikey "error.unbalancedAspects")
    | otherwise -> either (sendResponseStatus status400 . unknownCards) pure (checkedDeckList deckList)
 where
  unknownCards missing = ikeyWith "error.unknownCards" [textVar "cards" (intercalate ", " (map unCardCode missing))]

-- | Refuse decks that would put the same hero at the table twice.
refuseSharedHeroes :: [DeckList] -> Handler ()
refuseSharedHeroes deckLists = case sharedHeroes deckLists of
  [] -> pure ()
  titles -> sendResponseStatus status400 (ikeyWith "error.sharedHeroes" [textVar "heroes" (intercalate ", " titles)])

-- | Your seats in multi-handed solo, numbered when there are several.
soloSeats :: Text -> NonEmpty DeckList -> NonEmpty (Text, DeckList)
soloSeats name = \case
  deckList :| [] -> (name, deckList) :| []
  deckLists -> NE.zipWith (\n deckList -> (name <> " (" <> tshow n <> ")", deckList)) (NE.iterate (+ 1) (1 :: Int)) deckLists

-- | The seats of a game with friends, in order, with who sits there and their deck.
-- Someone sitting in several seats is numbered in each, as in multi-handed solo.
friendSeats :: MarvelGameId -> Handler [(Text, DeckList)]
friendSeats gid = do
  seats <- runDB $ selectList [MarvelSeatGameId ==. gid] [Asc MarvelSeatPosition]
  let users = map ((.marvelSeatUserId) . entityVal) seats
  for (zip [0 :: Int ..] seats) \(n, Entity _ seat) -> do
    name <- maybe "" (.userUsername) <$> runDB (get seat.marvelSeatUserId)
    let theirs = filter (== seat.marvelSeatUserId) users
        name' = if length theirs > 1 then name <> " (" <> tshow (1 + length (filter (== seat.marvelSeatUserId) (take n users))) <> ")" else name
    (name',) <$> maybe (sendResponseStatus status400 ("a seat has no deck" :: Text)) (decodeOrFail "deck") seat.marvelSeatDeck

-- | Deal a new game from a seed and play it up to its first question, with a seat for
-- each named deck.
startGame :: ScenarioSetup -> Maybe Campaign -> NonEmpty (Text, DeckList) -> Int -> IO (Either Text ((Game, [Message], StdGen), [FrontendMessage]))
startGame setup campaign players seed = case lookupScenario setup of
  Nothing -> pure $ Left $ ikeyWith "error.noSuchScenario" [textVar "scenario" (unCardCode setup.scenarioCode)]
  Just scenario -> case sequence $ evalRand (traverse (\(name, d) -> fmap (setName name) <$> loadPlayer d) players) (mkStdGen seed) of
    Left missing -> pure $ Left $ ikeyWith "error.unknownCards" [textVar "cards" (intercalate ", " (map unCardCode missing))]
    Right seated -> do
      (app, said) <- appFor (newTable scenario seated & campaignL .~ campaign) [StartGame] (mkStdGen $ seed + 1)
      runGameApp app runGame
      Right <$> ((,) <$> currentState app <*> (reverse <$> readIORef said))

postGamesR :: Handler Value
postGamesR = do
  userId <- requireUserId
  user <- runDB $ get404 userId
  new <- requireCheckJsonBody @_ @NewGame
  -- you play under your username
  let playerName = user.userUsername
  setup <- maybe (sendResponseStatus status400 (ikey "error.noScenario")) pure (new.scenario <|> recommendedSetup "01094")
  when (isNothing $ lookupScenario setup) $ sendResponseStatus status400 (ikeyWith "error.noSuchScenario" [textVar "scenario" (unCardCode setup.scenarioCode)])
  deckLists <- traverse (resolveDeck userId) new.decks
  refuseSharedHeroes deckLists
  let seatCount = fromMaybe (length deckLists) new.seats
      -- seats left open are for friends, and the game waits for them
      friends = seatCount > length deckLists
  when (seatCount < max 1 (length deckLists) || seatCount > 4) $ sendResponseStatus status400 (ikey "error.seatCount")
  now <- liftIO getCurrentTime
  let newGame' g queue gen =
        MarvelGame
          { marvelGameUserId = userId
          , marvelGameName = new.name
          , marvelGamePlayerName = playerName
          , marvelGameCurrentData = g
          , marvelGameCurrentQueue = queue
          , marvelGameRandomGen = showGen gen
          , marvelGameStep = 0
          , marvelGameScenario = toJSON setup
          , marvelGameDeck = toJSON <$> (guard (not friends) $> deckLists)
          , marvelGameAllowSpectators = fromMaybe False new.allowSpectators
          , marvelGameWithFriends = friends
          , marvelGameSeats = seatCount
          , marvelGameCampaign = Nothing
          , marvelGameCreatedAt = now
          , marvelGameUpdatedAt = now
          }
  gid <- case nonEmpty deckLists of
    Just dealt | not friends -> do
      seed <- maybe (liftIO randomIO) pure new.seed
      ((g, queue, gen), said) <- either (sendResponseStatus status400) pure =<< liftIO (startGame setup Nothing (soloSeats playerName dealt) seed)
      runDB do
        gid <- insert $ newGame' (toJSON g) (toJSON queue) gen
        void $ storeLog gid (-1) now said
        pure gid
    _ -> -- dealt once the rest of the seats are taken
      runDB do
        gid <- insert $ newGame' Null (toJSON ([] :: [Message])) (mkStdGen 0)
        for_ (zip [0 ..] deckLists) \(position, deckList) -> insert_ $ MarvelSeat gid position userId (Just $ toJSON deckList)
        pure gid
  pure $ object ["id" .= gid]

-- | Take the first free seat at a game with friends, bringing a deck. The owner may take
-- several, anyone else one. Taking the last one deals the game.
postJoinR :: MarvelGameId -> Handler ()
postJoinR gid = do
  userId <- requireUserId
  row <- runDB $ get404 gid
  unless (row.marvelGameWithFriends && awaitingPlayers row) $ sendResponseStatus status409 (ikey "error.notWaitingForPlayers")
  joining <- requireCheckJsonBody @_ @JoinGame
  deckList <- resolveDeck userId joining.deck
  seated <- runDB $ selectList [MarvelSeatGameId ==. gid] []
  when (row.marvelGameUserId /= userId && any ((== userId) . (.marvelSeatUserId) . entityVal) seated) $ sendResponseStatus status409 (ikey "error.alreadySeated")
  refuseSharedHeroes . (deckList :) . map snd =<< friendSeats gid
  case [0 .. row.marvelGameSeats - 1] \\ map ((.marvelSeatPosition) . entityVal) seated of
    [] -> sendResponseStatus status409 (ikey "error.noFreeSeat")
    (position : rest) -> do
      runDB $ insert_ $ MarvelSeat gid position userId (Just $ toJSON deckList)
      if null rest
        then deal gid row =<< maybe (error "no seats") pure . nonEmpty =<< friendSeats gid
        else broadcast gid row Nothing Nothing

-- | Give up your seats at a game with friends before it is dealt.
postLeaveR :: MarvelGameId -> Handler ()
postLeaveR gid = do
  userId <- requireUserId
  row <- runDB $ get404 gid
  unless (awaitingPlayers row) $ sendResponseStatus status409 (ikey "error.notWaitingForPlayers")
  runDB $ deleteWhere [MarvelSeatGameId ==. gid, MarvelSeatUserId ==. userId]
  broadcast gid row Nothing Nothing

-- | Deal a game with friends now, with the seats taken so far, closing the empty ones.
-- The seats are numbered again from 0 so they still match the players dealt.
postStartR :: MarvelGameId -> Handler ()
postStartR gid = do
  row <- requireOwnedGame gid
  unless (awaitingPlayers row) $ sendResponseStatus status409 (ikey "error.notWaitingForPlayers")
  seats <- runDB $ selectList [MarvelSeatGameId ==. gid] [Asc MarvelSeatPosition]
  when (null seats) $ sendResponseStatus status409 (ikey "error.noSeatTaken")
  row' <- runDB do
    -- out of the way first, as positions are unique
    for_ (zip [0 :: Int ..] seats) \(n, Entity sid _) -> update sid [MarvelSeatPosition =. -1 - n]
    for_ (zip [0 :: Int ..] seats) \(n, Entity sid _) -> update sid [MarvelSeatPosition =. n]
    updateGet gid [MarvelGameSeats =. length seats]
  deal gid row' =<< maybe (error "no seats") pure . nonEmpty =<< friendSeats gid

-- | Deal the game again from the start, with a new seed.
deal :: MarvelGameId -> MarvelGame -> NonEmpty (Text, DeckList) -> Handler ()
deal gid row players = do
  setup <- decodeOrFail "scenario" row.marvelGameScenario
  campaign <- traverse (decodeOrFail "campaign") row.marvelGameCampaign
  ((g, queue, gen), said) <- either (sendResponseStatus status400) pure =<< liftIO . startGame setup campaign players =<< liftIO randomIO
  now <- liftIO getCurrentTime
  (row', entries) <- runDB do
    deleteWhere [MarvelStepGameId ==. gid]
    clearLog gid
    void $ storeLog gid (-1) now said
    (,)
      <$> updateGet
        gid
        [ MarvelGameCurrentData =. toJSON g
        , MarvelGameCurrentQueue =. toJSON queue
        , MarvelGameRandomGen =. showGen gen
        , MarvelGameStep =. 0
        , MarvelGameUpdatedAt =. now
        ]
      <*> logTail gid
  broadcast gid row' (Just g) (Just entries)

handleSocket :: Maybe UserId -> TChan Outgoing -> WebSocketsT Handler ()
handleSocket viewer chan = do
  readChannel <- atomically $ dupTChan chan
  forever $ atomically (readTChan readChannel) >>= \case
    Said bytes -> sendTextData bytes
    Told table seatOrder e -> when (readsEntry (seesHand table viewer seatOrder) e) $ sendTextData (encode (Logged e))
    Updated row table g entries -> sendTextData =<< updateFor viewer row table g entries

-- | The game as it stands, or its updates as they come over a websocket.
serveGame :: MarvelGameId -> MarvelGame -> Handler Value
serveGame gid row = do
  viewer <- lookupUserId
  webSockets . handleSocket viewer =<< gameChannel gid
  table <- tableFor gid row
  entries <- runDB $ logTail gid
  g <- storedGame row
  toJSON <$> gameData viewer row table g (Just entries)

getGameR :: MarvelGameId -> Handler Value
getGameR gid = serveGame gid =<< requirePlayableGame gid

-- | The same game and updates for someone watching. The socket only sends, and
-- answering needs a seat, so watching cannot change the game.
getSpectateR :: MarvelGameId -> Handler Value
getSpectateR gid = serveGame gid =<< requireWatchableGame gid

-- | Games in progress open to spectators, other than your own.
getSpectatableR :: Handler Value
getSpectatableR = do
  userId <- lookupUserId
  games <- runDB $ selectList ((MarvelGameAllowSpectators ==. True) : maybe [] (\u -> [MarvelGameUserId !=. u]) userId) [Desc MarvelGameUpdatedAt, LimitTo 50]
  pure $ toJSON $ filter (isNothing . (.outcome)) $ mapMaybe summary games

data AdminDashboard = AdminDashboard
  { users :: Int
  , activeUsers :: Int
  , games :: Int
  , recentGames :: [GameSummary]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- | Counts of users and games, active meaning a game played in the last two weeks, and
-- the games played most recently.
getAdminR :: Handler Value
getAdminR = do
  requireAdmin
  since <- addUTCTime (-14 * nominalDay) <$> liftIO getCurrentTime
  runDB do
    users <- Database.Persist.Postgresql.count ([] :: [Filter User])
    games <- Database.Persist.Postgresql.count ([] :: [Filter MarvelGame])
    active <- rawSql "SELECT COUNT(DISTINCT user_id) FROM marvel_games WHERE updated_at >= ?" [toPersistValue since]
    recent <- selectList [] [Desc MarvelGameUpdatedAt, LimitTo 20]
    pure $ toJSON AdminDashboard {users, activeUsers = sum (map unSingle active), games, recentGames = mapMaybe summary recent}

data Answer = Answer
  { choice :: Int
  , player :: IdentityId
  , questionVersion :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

postGameR :: MarvelGameId -> Handler ()
postGameR gid = do
  row <- requirePlayableGame gid
  userId <- requireUserId
  seats <- runDB $ selectList [MarvelSeatGameId ==. gid, MarvelSeatUserId ==. userId] []
  answer <- requireCheckJsonBody @_ @Answer
  advance gid row \before -> do
    when (answer.questionVersion /= gameQuestionVersion before) $
      sendResponseStatus status409 ("stale answer" :: Text)
    -- only the seat asked can answer, and with friends only from its own player
    unless (HashMap.member answer.player (gameQuestion before)) $
      sendResponseStatus status409 ("not asked" :: Text)
    let yours = mapMaybe ((gameSeats before `atMay`) . (.marvelSeatPosition) . entityVal) seats
    unless (if row.marvelGameWithFriends then answer.player `elem` yours else row.marvelGameUserId == userId) notFound
    pure $ answerQuestion answer.player answer.choice

-- | Run any engine message against the game, for adjusting it by hand while testing.
postDebugR :: MarvelGameId -> Handler ()
postDebugR gid = do
  row <- requireOwnedGame gid
  msg <- requireCheckJsonBody @_ @Message
  advance gid row \_ -> pure $ runDebug msg

-- | Play a stored game on from where it stands, keeping the move as an undoable step.
advance :: MarvelGameId -> MarvelGame -> (Game -> Handler (GameAppT ())) -> Handler ()
advance gid row prepare = do
  (app, said) <- loadGame row
  (before, queue, gen) <- liftIO $ currentState app
  play <- prepare before
  liftIO (try $ runGameApp app play) >>= \case
    Left (err :: SomeException) -> sendResponseStatus status500 ("the engine failed: " <> tshow err)
    Right () -> pure ()
  (after, queue', gen') <- liftIO $ currentState app
  now <- liftIO getCurrentTime
  let step = row.marvelGameStep + 1
  (row', stored) <- runDB do
    stored <- storeLog gid row.marvelGameStep now . reverse =<< readIORef said
    insert_
      MarvelStep
        { marvelStepGameId = gid
        , marvelStepStep = step
        , marvelStepPatchDown = toJSON $ Diff.diff (toJSON after) (toJSON before)
        , marvelStepQueue = toJSON queue
        , marvelStepRandomGen = showGen gen
        , marvelStepRound = gameRound before
        , marvelStepPhase = tshow (gamePhase before)
        , marvelStepCreatedAt = now
        }
    updateGet
      gid
      [ MarvelGameCurrentData =. toJSON after
      , MarvelGameCurrentQueue =. toJSON queue'
      , MarvelGameRandomGen =. showGen gen'
      , MarvelGameStep =. step
      , MarvelGameUpdatedAt =. now
      ]
      <&> (,stored)
  publish gid row' (gameSeats after) stored
  broadcast gid row' (Just after) Nothing

deleteGameR :: MarvelGameId -> Handler ()
deleteGameR gid = requireOwnedGame gid >> runDB (delete gid)

-- | The page of the log before the entry given, for scrolling back.
getLogR :: MarvelGameId -> Handler Value
getLogR gid = do
  row <- requireWatchableGame gid
  viewer <- lookupUserId
  before <- maybe (sendResponseStatus status400 ("before what?" :: Text)) pure . (readMaybe . unpack =<<) =<< lookupGetParam "before"
  table <- tableFor gid row
  g <- storedGame row
  entries <- runDB $ logBefore gid before
  pure $ toJSON $ filter (readsEntry $ seesHand table viewer (maybe [] gameSeats g)) entries

newtype Say = Say {text :: Text}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

-- | Something a player says, into the log for the table to read.
postChatR :: MarvelGameId -> Handler ()
postChatR gid = do
  row <- requirePlayableGame gid
  user <- runDB . get404 =<< requireUserId
  said <- T.strip . (.text) <$> requireCheckJsonBody @_ @Say
  when (null said || length said > 500) $ sendResponseStatus status400 ("say something shorter" :: Text)
  now <- liftIO getCurrentTime
  stored <- runDB $ storeLog gid row.marvelGameStep now [Logged $ chat [LogText user.userUsername, LogText ": ", LogText said]]
  g <- storedGame row
  publish gid row (maybe [] gameSeats g) stored

getDecksR :: Handler Value
getDecksR = do
  userId <- requireUserId
  db <- getsYesod (.cardDb)
  toJSON <$> runDB (listDecks db userId)

-- | Import a deck from a MarvelCDB link or a deck file's contents.
postDecksR :: Handler Value
postDecksR = do
  userId <- requireUserId
  request <- requireCheckJsonBody @_ @ImportRequest
  App {cardDb, manager} <- getYesod
  liftIO (importDeck manager request) >>= \case
    Left err -> sendResponseStatus status400 (describeImportError err)
    Right parsed
      | savesDeck request -> toJSON <$> runDB (saveDeck cardDb userId parsed)
      | otherwise -> pure $ toJSON $ previewDeck cardDb parsed

getDeckR :: MarvelDeckId -> Handler Value
getDeckR deckId = do
  userId <- requireUserId
  db <- getsYesod (.cardDb)
  maybe notFound (pure . toJSON) =<< runDB (loadDeck db userId deckId)

deleteDeckR :: MarvelDeckId -> Handler ()
deleteDeckR deckId = do
  userId <- requireUserId
  deck <- runDB $ get deckId
  unless (((.marvelDeckUserId) <$> deck) == Just userId) notFound
  runDB $ delete deckId

-- | The core set's starter decks, to play without importing anything.
getStarterDecksR :: Handler Value
getStarterDecksR = pure $ toJSON [object ["key" .= key, "hero" .= d.hero, "aspects" .= d.aspects] | (key, d) <- starterDecks]

-- | The scenarios there are, each with the modular set it recommends, and the modular
-- sets any of them may be played with. The client finds their releases in the card data.
getScenariosR :: Handler Value
getScenariosR =
  pure $
    object
      [ "scenarios"
          .= [ object ["code" .= s.scenarioCode, "name" .= fst (villainOf s), "modulars" .= s.modulars]
             | Just s <- map recommendedSetup scenarioCodes
             ]
      , "modulars" .= modularSets
      ]

-- * Campaigns

data NewCampaign = NewCampaign
  { name :: Text
  , mode :: CampaignMode
  , difficulty :: Maybe Difficulty
  -- ^ each scenario's, standard unless expert
  , decks :: [DeckChoice]
  -- ^ one per seat you take
  , seats :: Maybe Int
  -- ^ how many play, at least one per deck; the rest are open for friends, as in a game
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

-- | A campaign as its overview shows it: the log as it stands, counting the game just
-- won, who plays it, and what is played next.
data CampaignView = CampaignView
  { id :: MarvelCampaignId
  , name :: Text
  , campaign :: Campaign
  , difficulty :: Difficulty
  , heroes :: [CardCode]
  , table :: Table
  -- ^ its seats, as a game's lobby shows them
  , waitingForPlayers :: Bool
  -- ^ with friends, seats still open before its first game
  , deckShort :: [Int]
  -- ^ for each seat taken, how many cards its deck must gain to be dealt (rulebook p.12)
  , next :: Maybe CardCode
  , nextName :: Maybe Text
  , currentGame :: Maybe MarvelGameId
  -- ^ the game of it being played, not yet over
  , won :: Bool
  , updatedAt :: UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- | The campaign's seats in order: who sits there and the deck they began it with. One
-- made before campaigns had seats has its owner in a seat for each of its decks.
campaignSeats :: MarvelCampaignId -> MarvelCampaign -> Handler [(Int, UserId, DeckList)]
campaignSeats cid row = do
  seats <- runDB $ selectList [MarvelCampaignSeatCampaignId ==. cid] [Asc MarvelCampaignSeatPosition]
  if null seats && not row.marvelCampaignWithFriends
    then zip3 [0 ..] (repeat row.marvelCampaignUserId) <$> decodeOrFail @[DeckList] "decks" row.marvelCampaignDecks
    else for seats \(Entity _ s) -> (s.marvelCampaignSeatPosition,s.marvelCampaignSeatUserId,) <$> decodeOrFail "deck" s.marvelCampaignSeatDeck

-- | Each seat's player by name, numbered when someone sits in several, as at a game.
seatNames :: [(Int, UserId, DeckList)] -> Handler [(UserId, Text, DeckList)]
seatNames seats = for (zip [0 :: Int ..] seats) \(n, (_, userId, deck)) -> do
  name <- maybe "" (.userUsername) <$> runDB (get userId)
  let theirs = [u | (_, u, _) <- seats, u == userId]
      before = length [u | (_, u, _) <- take n seats, u == userId]
  pure (userId, if length theirs > 1 then name <> " (" <> tshow (before + 1) <> ")" else name, deck)

-- | The campaign is waiting for friends to take its open seats: before its first game.
awaitingCampaignPlayers :: MarvelCampaign -> Campaign -> Int -> Bool
awaitingCampaignPlayers row c taken =
  row.marvelCampaignWithFriends && c.completed == 0 && isNothing row.marvelCampaignCurrentGame && taken < row.marvelCampaignSeats

-- | A campaign you play: your own, or one you have a seat in. One with friends still
-- waiting for players is open to anyone signed in, so its invite link shows its seats.
requireCampaignMember :: MarvelCampaignId -> Handler MarvelCampaign
requireCampaignMember cid = do
  userId <- requireUserId
  row <- runDB $ get404 cid
  seats <- campaignSeats cid row
  c <- decodeOrFail "campaign" row.marvelCampaignCampaign
  unless (row.marvelCampaignUserId == userId || any (\(_, u, _) -> u == userId) seats || awaitingCampaignPlayers row c (length seats)) notFound
  pure row

requireOwnedCampaign :: MarvelCampaignId -> Handler MarvelCampaign
requireOwnedCampaign cid = do
  userId <- requireUserId
  row <- runDB $ get404 cid
  unless (row.marvelCampaignUserId == userId) notFound
  pure row

-- | The campaign with its game's result written in: the log the game won, or as it was
-- for a game lost (the scenario played again, no penalty), unless losing lost the
-- campaign. The game still being played, if it is.
campaignNow :: MarvelCampaign -> Handler (Campaign, Maybe MarvelGameId)
campaignNow row = do
  stored <- decodeOrFail "campaign" row.marvelCampaignCampaign
  game <- runDB $ traverse (\gid -> fmap (gid,) <$> get gid) row.marvelCampaignCurrentGame
  case join game of
    Nothing -> pure (stored, Nothing)
    Just (gid, g) -> case fromJSON g.marvelGameCurrentData of
      Success (game' :: Game) -> case gameState game' of
        Finished Won -> pure (fromMaybe stored (gameCampaign game'), Nothing)
        Finished Lost -> pure (maybe stored (\c -> if c.lost then c else stored) (gameCampaign game'), Nothing)
        _ -> pure (stored, Just gid)
      Error _ -> pure (stored, Just gid)

campaignView :: Entity MarvelCampaign -> Handler CampaignView
campaignView (Entity cid row) = do
  (c, current) <- campaignNow row
  difficulty <- decodeOrFail "difficulty" row.marvelCampaignDifficulty
  seats <- campaignSeats cid row
  seated <- for seats \(position, userId, deck) -> do
    name <- maybe "" (.userUsername) <$> runDB (get userId)
    pure $ SeatView position userId name (Just deck.hero)
  let next = currentScenario c
  pure
    CampaignView
      { id = cid
      , name = row.marvelCampaignName
      , campaign = c
      , difficulty
      , heroes = [d.hero | (_, _, d) <- seats]
      , table = Table {withFriends = row.marvelCampaignWithFriends, seats = row.marvelCampaignSeats, owner = row.marvelCampaignUserId, seated}
      , waitingForPlayers = awaitingCampaignPlayers row c (length seats)
      , deckShort = [campaignDeckShortBy c d | (_, _, d) <- seats]
      , next
      , nextName = fst . villainOf . (\code -> ScenarioSetup code [] difficulty) <$> next
      , currentGame = current
      , won = campaignWon c
      , updatedAt = row.marvelCampaignUpdatedAt
      }

-- | The campaigns you own or have a seat in.
getCampaignsR :: Handler Value
getCampaignsR = do
  userId <- requireUserId
  rows <- runDB do
    seatedIn <- map ((.marvelCampaignSeatCampaignId) . entityVal) <$> selectList [MarvelCampaignSeatUserId ==. userId] []
    selectList ([MarvelCampaignUserId ==. userId] ||. [MarvelCampaignId <-. seatedIn]) [Desc MarvelCampaignUpdatedAt]
  toJSON <$> traverse campaignView rows

-- | A new campaign, its players' heroes recorded in its log. Alone, its first game is
-- dealt when it is continued; with seats left open for friends, once they are taken.
postCampaignsR :: Handler Value
postCampaignsR = do
  userId <- requireUserId
  new <- requireCheckJsonBody @_ @NewCampaign
  deckLists <- traverse (resolveDeck userId) new.decks
  refuseSharedHeroes deckLists
  let seatCount = fromMaybe (length deckLists) new.seats
  when (seatCount < max 1 (length deckLists) || seatCount > 4) $ sendResponseStatus status400 (ikey "error.seatCount")
  now <- liftIO getCurrentTime
  cid <- runDB do
    cid <-
      insert
        MarvelCampaign
          { marvelCampaignUserId = userId
          , marvelCampaignName = new.name
          , marvelCampaignCampaign = toJSON $ newCampaign new.mode (map (.hero) deckLists)
          , marvelCampaignDecks = toJSON ([] :: [DeckList])
          , marvelCampaignDifficulty = toJSON (fromMaybe Normal new.difficulty)
          , marvelCampaignCurrentGame = Nothing
          , marvelCampaignWithFriends = seatCount > length deckLists
          , marvelCampaignSeats = seatCount
          , marvelCampaignCreatedAt = now
          , marvelCampaignUpdatedAt = now
          }
    for_ (zip [0 ..] deckLists) \(position, deck) -> insert_ $ MarvelCampaignSeat cid position userId (toJSON deck)
    pure cid
  pure $ object ["id" .= cid]

getCampaignR :: MarvelCampaignId -> Handler Value
getCampaignR cid = do
  row <- requireCampaignMember cid
  toJSON <$> campaignView (Entity cid row)

deleteCampaignR :: MarvelCampaignId -> Handler ()
deleteCampaignR cid = requireOwnedCampaign cid >> runDB (delete cid)

-- | The log's players are the seats' heroes, in seat order: while seats are taken and
-- left before the first game, as the identities are recorded as it is set up.
reseat :: MarvelCampaignId -> MarvelCampaign -> Handler ()
reseat cid row = do
  c <- decodeOrFail @Campaign "campaign" row.marvelCampaignCampaign
  seats <- campaignSeats cid row
  let log' = (newCampaign c.mode [d.hero | (_, _, d) <- seats]).campaignLog
  now <- liftIO getCurrentTime
  runDB $ update cid [MarvelCampaignCampaign =. toJSON c {campaignLog = log'}, MarvelCampaignUpdatedAt =. now]

-- | Take the first open seat of a campaign with friends, bringing a deck, as at a game.
-- Taking the last one deals its first scenario.
postJoinCampaignR :: MarvelCampaignId -> Handler Value
postJoinCampaignR cid = do
  userId <- requireUserId
  row <- runDB $ get404 cid
  c <- decodeOrFail @Campaign "campaign" row.marvelCampaignCampaign
  seats <- campaignSeats cid row
  unless (awaitingCampaignPlayers row c (length seats)) $ sendResponseStatus status409 (ikey "error.notWaitingForPlayers")
  joining <- requireCheckJsonBody @_ @JoinGame
  deckList <- resolveDeck userId joining.deck
  when (row.marvelCampaignUserId /= userId && any (\(_, u, _) -> u == userId) seats) $ sendResponseStatus status409 (ikey "error.alreadySeated")
  refuseSharedHeroes (deckList : [d | (_, _, d) <- seats])
  case [0 .. row.marvelCampaignSeats - 1] \\ [p | (p, _, _) <- seats] of
    [] -> sendResponseStatus status409 (ikey "error.noFreeSeat")
    (position : rest) -> do
      runDB $ insert_ $ MarvelCampaignSeat cid position userId (toJSON deckList)
      reseat cid row
      if null rest
        then (\gid -> object ["gameId" .= gid]) <$> dealCampaignGame cid
        else pure $ object []

-- | Give up your seats before the campaign's first game.
postLeaveCampaignR :: MarvelCampaignId -> Handler ()
postLeaveCampaignR cid = do
  userId <- requireUserId
  row <- runDB $ get404 cid
  c <- decodeOrFail @Campaign "campaign" row.marvelCampaignCampaign
  seats <- campaignSeats cid row
  unless (awaitingCampaignPlayers row c (length seats)) $ sendResponseStatus status409 (ikey "error.notWaitingForPlayers")
  runDB $ deleteWhere [MarvelCampaignSeatCampaignId ==. cid, MarvelCampaignSeatUserId ==. userId]
  reseat cid row

-- | Begin the campaign with the seats taken so far, closing the empty ones, as a game
-- with friends can be started: its first scenario is dealt.
postStartCampaignR :: MarvelCampaignId -> Handler Value
postStartCampaignR cid = do
  row <- requireOwnedCampaign cid
  c <- decodeOrFail @Campaign "campaign" row.marvelCampaignCampaign
  seats <- runDB $ selectList [MarvelCampaignSeatCampaignId ==. cid] [Asc MarvelCampaignSeatPosition]
  unless (awaitingCampaignPlayers row c (length seats)) $ sendResponseStatus status409 (ikey "error.notWaitingForPlayers")
  when (null seats) $ sendResponseStatus status409 (ikey "error.noSeatTaken")
  row' <- runDB do
    -- out of the way first, as positions are unique
    for_ (zip [0 :: Int ..] seats) \(n, Entity sid _) -> update sid [MarvelCampaignSeatPosition =. -1 - n]
    for_ (zip [0 :: Int ..] seats) \(n, Entity sid _) -> update sid [MarvelCampaignSeatPosition =. n]
    updateGet cid [MarvelCampaignSeats =. length seats]
  reseat cid row'
  (\gid -> object ["gameId" .= gid]) <$> dealCampaignGame cid

newtype SeatDeck = SeatDeck {deck :: DeckChoice}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

-- | Change the deck you play a seat with, between the campaign's games: the same hero,
-- as "players are free to change aspects and alter the contents of their deck" but not
-- their identity (rulebook p.3). This is how a deck the campaign took allies from is
-- brought back up to size.
putCampaignSeatR :: MarvelCampaignId -> Int -> Handler ()
putCampaignSeatR cid position = do
  userId <- requireUserId
  row <- requireCampaignMember cid
  (_, current) <- campaignNow row
  when (isJust current) $ sendResponseStatus status409 (ikey "error.campaignGameInProgress")
  seats <- campaignSeats cid row
  (_, owner, deck) <- maybe notFound pure $ find (\(p, _, _) -> p == position) seats
  unless (owner == userId) notFound
  SeatDeck choice <- requireCheckJsonBody
  deck' <- resolveDeck userId choice
  unless (toHeroCardCode deck'.hero == toHeroCardCode deck.hero) $ sendResponseStatus status400 (ikey "error.campaignSameHero")
  now <- liftIO getCurrentTime
  runDB do
    seated <- selectFirst [MarvelCampaignSeatCampaignId ==. cid, MarvelCampaignSeatPosition ==. position] []
    case seated of
      Just (Entity sid _) -> update sid [MarvelCampaignSeatDeck =. toJSON deck']
      -- a campaign from before seats: its decks kept on it
      Nothing -> update cid [MarvelCampaignDecks =. toJSON [if p == position then deck' else d | (p, _, d) <- seats]]
    update cid [MarvelCampaignUpdatedAt =. now]

-- | Back to the campaign's game being played, or the next scenario dealt from the log.
postContinueCampaignR :: MarvelCampaignId -> Handler Value
postContinueCampaignR cid = do
  row <- requireCampaignMember cid
  (_, current) <- campaignNow row
  gid <- maybe (dealCampaignGame cid) pure current
  pure $ object ["gameId" .= gid]

-- | Deal the campaign's next scenario: to each seat the deck its player began the
-- campaign with and the cards the log adds. Each deck must be big enough once the
-- allies removed from the campaign are out of it: "you must add a card to your deck"
-- (rulebook p.12).
dealCampaignGame :: MarvelCampaignId -> Handler MarvelGameId
dealCampaignGame cid = do
  row <- runDB $ get404 cid
  (c, _) <- campaignNow row
  code <- maybe (sendResponseStatus status409 (ikey "error.campaignOver")) pure (currentScenario c)
  difficulty <- decodeOrFail @Difficulty "difficulty" row.marvelCampaignDifficulty
  setup <- maybe (sendResponseStatus status400 (ikeyWith "error.noSuchScenario" [textVar "scenario" (unCardCode code)])) pure (recommendedSetup code)
  seats <- campaignSeats cid row
  when (length seats < row.marvelCampaignSeats) $ sendResponseStatus status409 (ikey "error.notAllSeatsTaken")
  for_ seats \(_, _, d) -> do
    let short = campaignDeckShortBy c d
    when (short > 0) $
      sendResponseStatus status409 (ikeyWith "error.campaignDeckTooSmall" [textVar "hero" (heroName d.hero), textVar "count" (tshow short)])
  players <- maybe (sendResponseStatus status400 ("the campaign has no decks" :: Text)) pure . nonEmpty =<< seatNames seats
  owner <- runDB $ get404 row.marvelCampaignUserId
  let setup' = ScenarioSetup setup.scenarioCode setup.modulars difficulty
      named = c {campaignName = Just row.marvelCampaignName}
      dealt = fmap (\(u, n, d) -> (u, n, campaignDeck c d)) players
      friends = row.marvelCampaignWithFriends
  ((g, queue, gen), said) <-
    either (sendResponseStatus status400) pure =<< liftIO . startGame setup' (Just named) (fmap (\(_, n, d) -> (n, d)) dealt) =<< liftIO randomIO
  now <- liftIO getCurrentTime
  runDB do
    gid <-
      insert
        MarvelGame
          { marvelGameUserId = row.marvelCampaignUserId
          , marvelGameName = row.marvelCampaignName <> " – " <> fst (villainOf setup')
          , marvelGamePlayerName = owner.userUsername
          , marvelGameCurrentData = toJSON g
          , marvelGameCurrentQueue = toJSON queue
          , marvelGameRandomGen = showGen gen
          , marvelGameStep = 0
          , marvelGameScenario = toJSON setup'
          , marvelGameDeck = if friends then Nothing else Just (toJSON [d | (_, _, d) <- toList dealt])
          , marvelGameAllowSpectators = False
          , marvelGameWithFriends = friends
          , marvelGameSeats = length dealt
          , marvelGameCampaign = Just (toJSON named)
          , marvelGameCreatedAt = now
          , marvelGameUpdatedAt = now
          }
    -- with friends, each plays their own seat of it
    when friends $ for_ (zip [0 ..] (toList dealt)) \(position, (u, _, d)) -> insert_ $ MarvelSeat gid position u (Just $ toJSON d)
    void $ storeLog gid (-1) now said
    update cid [MarvelCampaignCampaign =. toJSON c, MarvelCampaignCurrentGame =. Just gid, MarvelCampaignUpdatedAt =. now]
    pure gid
 where
  heroName code = maybe (unCardCode code) (display . cdName) (HashMap.lookup (toHeroCardCode code) allHeroesMap)

data UndoTarget = LastAnswer | StartOfPhase | StartOfRound | ToStep Int
  deriving stock (Eq)

-- | How many steps to take back: one answer, every answer since the phase or round
-- began, or every answer since the step given, as a log entry offers.
stepsToUndo :: UndoTarget -> [MarvelStep] -> Int
stepsToUndo target latestFirst = case (target, latestFirst) of
  (_, []) -> 0
  (LastAnswer, _) -> 1
  (ToStep n, _) -> length $ takeWhile ((> n) . (.marvelStepStep)) latestFirst
  (StartOfPhase, newest : _) -> length $ takeWhile (samePhase newest) latestFirst
  (StartOfRound, newest : _) -> length $ takeWhile (sameRound newest) latestFirst
 where
  samePhase a b = sameRound a b && a.marvelStepPhase == b.marvelStepPhase
  sameRound a b = a.marvelStepRound == b.marvelStepRound

-- | Step a game back by applying the latest steps' patches, newest first.
postUndoR :: MarvelGameId -> Handler ()
postUndoR gid = do
  row <- requirePlayableGame gid
  stepParam <- (readMaybe . unpack =<<) <$> lookupGetParam "step"
  target <-
    lookupGetParam "to" <&> \case
      Just "phase" -> StartOfPhase
      Just "round" -> StartOfRound
      Just "step" | Just n <- stepParam -> ToStep n
      _ -> LastAnswer
  steps <- runDB $ selectList [MarvelStepGameId ==. gid] [Desc MarvelStepStep]
  let undone = take (stepsToUndo target (map entityVal steps)) steps
  for_ (lastMay undone) \(Entity _ oldest) -> do
    previous <- foldM undoStep row.marvelGameCurrentData undone
    g <- decodeOrFail "game" previous
    now <- liftIO getCurrentTime
    let step = row.marvelGameStep - length undone
    (row', entries) <- runDB do
      deleteWhere [MarvelStepId <-. map entityKey undone]
      trimLog gid step
      (,)
        <$> updateGet
          gid
          [ MarvelGameCurrentData =. previous
          , MarvelGameCurrentQueue =. oldest.marvelStepQueue
          , MarvelGameRandomGen =. oldest.marvelStepRandomGen
          , MarvelGameStep =. step
          , MarvelGameUpdatedAt =. now
          ]
        <*> logTail gid
    broadcast gid row' (Just g) (Just entries)
 where
  undoStep current (Entity _ step) = do
    patch <- decodeOrFail "undo patch" step.marvelStepPatchDown
    case Diff.patch patch current of
      Success v -> pure v
      Error err -> sendResponseStatus status400 ("could not undo: " <> pack @Text err)

postResetR :: MarvelGameId -> Handler ()
postResetR gid = do
  row <- requireOwnedGame gid
  when (awaitingPlayers row) $ sendResponseStatus status409 (ikey "error.notDealtYet")
  players <-
    if row.marvelGameWithFriends
      then maybe (error "no seats") pure . nonEmpty =<< friendSeats gid
      else do
        decks <- maybe (sendResponseStatus status400 ("the game has no decks" :: Text)) (decodeOrFail "deck") row.marvelGameDeck
        soloSeats row.marvelGamePlayerName <$> maybe (sendResponseStatus status400 ("the game has no decks" :: Text)) pure (nonEmpty decks)
  deal gid row players

-- | Everything needed to carry a game elsewhere, undo history included.
data ExportedGame = ExportedGame
  { version :: Int
  , name :: Text
  , playerName :: Text
  , scenario :: Value
  , game :: Value
  , queue :: Value
  , randomGen :: Text
  , step :: Int
  , steps :: [ExportedStep]
  , log :: Maybe [LogEntry]
  -- ^ every entry, oldest first
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data ExportedStep = ExportedStep
  { step :: Int
  , patchDown :: Value
  , queue :: Value
  , randomGen :: Text
  , round :: Int
  , phase :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

exportVersion :: Int
exportVersion = 1

-- | A game the signed-in user plays, as an export.
exportGame :: MarvelGameId -> Handler (MarvelGame, ExportedGame)
exportGame gid = do
  row <- requirePlayableGame gid
  (steps, entries) <- runDB $ (,) <$> selectList [MarvelStepGameId ==. gid] [Asc MarvelStepStep] <*> wholeLog gid
  pure
    ( row
    , ExportedGame
        { version = exportVersion
        , name = row.marvelGameName
        , playerName = row.marvelGamePlayerName
        , scenario = row.marvelGameScenario
        , game = row.marvelGameCurrentData
        , queue = row.marvelGameCurrentQueue
        , randomGen = row.marvelGameRandomGen
        , step = row.marvelGameStep
        , steps =
            [ ExportedStep st.marvelStepStep st.marvelStepPatchDown st.marvelStepQueue st.marvelStepRandomGen st.marvelStepRound st.marvelStepPhase
            | Entity _ st <- steps
            ]
        , log = Just entries
        }
    )

getExportR :: MarvelGameId -> Handler Value
getExportR gid = do
  (row, exported) <- exportGame gid
  addHeader "Content-Disposition" $ "attachment; filename=\"" <> slug row.marvelGameName <> ".json\""
  pure $ toJSON exported
 where
  slug = T.intercalate "-" . T.words . T.filter (\c -> c == ' ' || c == '-' || isAlphaNum c) . toLower

-- | Upload the game's export for a bug report, giving back the URL to put in the issue.
postBugR :: MarvelGameId -> Handler Value
postBugR gid = do
  (_, exported) <- exportGame gid
  manager <- getsYesod (.manager)
  liftIO (uploadBugExport manager (encode exported)) >>= \case
    Left err -> sendResponseStatus status500 err
    Right url -> pure $ object ["url" .= url]

-- | Load an exported game as a new game of the signed-in user's.
postImportR :: Handler Value
postImportR = do
  userId <- requireUserId
  exported <- requireCheckJsonBody @_ @ExportedGame
  unless (exported.version == exportVersion) $ sendResponseStatus status400 (ikey "error.unsupportedExport")
  -- refuse a file the engine cannot read before storing any of it
  imported <- decodeOrFail @Game "game" exported.game
  _ <- decodeOrFail @[Message] "queue" exported.queue
  _ <- decodeOrFail @ScenarioSetup "scenario" exported.scenario
  _ <- readGen exported.randomGen
  now <- liftIO getCurrentTime
  gid <- runDB do
    gid <-
      insert
        MarvelGame
          { marvelGameUserId = userId
          , marvelGameName = exported.name
          , marvelGamePlayerName = exported.playerName
          , marvelGameCurrentData = exported.game
          , marvelGameCurrentQueue = exported.queue
          , marvelGameRandomGen = exported.randomGen
          , marvelGameStep = exported.step
          , marvelGameScenario = exported.scenario
          , marvelGameDeck = Nothing
          , marvelGameAllowSpectators = False
          , marvelGameWithFriends = False
          , marvelGameSeats = length (gameSeats imported)
          , marvelGameCampaign = Nothing
          , marvelGameCreatedAt = now
          , marvelGameUpdatedAt = now
          }
    insertMany_
      [ MarvelStep gid st.step st.patchDown st.queue st.randomGen st.round st.phase now
      | st <- exported.steps
      ]
    restoreLog gid now (fromMaybe [] exported.log)
    pure gid
  pure $ object ["id" .= gid]

-- | Guests gone a month, with no game of theirs or at their seat played since, are
-- deleted, and everything of theirs with them.
sweepGuests :: (MonadIO m) => SqlPersistT m ()
sweepGuests = do
  cutoff <- addUTCTime (-30 * nominalDay) <$> liftIO getCurrentTime
  rawExecute
    "DELETE FROM users u WHERE u.guest AND u.created_at < ? AND NOT EXISTS \
    \(SELECT 1 FROM marvel_games g WHERE g.updated_at >= ? AND (g.user_id = u.id \
    \OR EXISTS (SELECT 1 FROM marvel_seats s WHERE s.game_id = g.id AND s.user_id = u.id)))"
    [toPersistValue cutoff, toPersistValue cutoff]

main :: IO ()
main = do
  port <- fromMaybe 3000 . (>>= readMaybe) <$> lookupEnv "PORT"
  database <- fromMaybe "host=localhost dbname=marvelchampions" <$> lookupEnv "DATABASE_URL"
  pool <- runStdoutLoggingT $ createPostgresqlPool (encodeUtf8 $ pack database) 10
  runSqlPool (runMigration migrateAll) pool
  void $ forkIO $ forever do
    runSqlPool sweepGuests pool
    threadDelay (24 * 60 * 60 * 1000000)
  channels <- newTVarIO mempty
  jwtSecret <- maybe "development-only-secret-set-JWT_SECRET-in-production" pack <$> lookupEnv "JWT_SECRET"
  -- HS256 signing needs a key of at least 256 bits
  when (length (encodeUtf8 jwtSecret) < 32) $ error "JWT_SECRET must be at least 32 bytes"
  cardsPath <- fromMaybe "frontend/public/data/cards.json" <$> lookupEnv "CARDS_JSON"
  cardDb <- loadCardDb cardsPath >>= either (\err -> error $ "could not load " <> cardsPath <> ": " <> err) pure
  manager <- newTlsManager
  warp port App {pool, jwtSecret, channels, cardDb, manager}
