-- | The wire format for a game-log entry, as in Arkham's "Arkham.Log.Entry".
--
-- A leaf: it imports only ids and card codes, so anything can import it. Names travel as
-- text; the typed builders live in "Marvel.Log".
module Marvel.Log.Entry (module Marvel.Log.Entry) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Marvel.Card.Code
import Marvel.Card.Id
import Marvel.Id
import Marvel.Prelude

-- | What a 'LogRef' points at, which is how the client decides how to draw it.
data LogRefKind
  = RefCard
  | RefPlayer
  | RefIdentity
  | RefAlly
  | RefSupport
  | RefUpgrade
  | RefEvent
  | RefVillain
  | RefMinion
  | RefTreachery
  | RefAttachment
  | RefObligation
  | RefMainScheme
  | RefSideScheme
  | RefScenario
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Something in the game, drawn as a chip the reader can hover for the card. One record
-- keyed on its kind, so a new kind changes nothing on the wire.
data LogRef = LogRef
  { logRefKind :: LogRefKind
  , logRefName :: Text
  -- ^ already displayable; the client shows the card's name in the player's language
  -- from the code when it has one
  , logRefCardCode :: Maybe CardCode
  , logRefCardId :: Maybe CardId
  , logRefEntityId :: Maybe Text
  -- ^ the in-play entity's id, as its JSON text
  , logRefFaceDown :: Bool
  -- ^ draw the back
  }
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON LogRef where
  toJSON = genericToJSON (aesonOptions $ Just "logRef")

instance FromJSON LogRef where
  parseJSON = genericParseJSON (aesonOptions $ Just "logRef")

logRef :: LogRefKind -> Text -> LogRef
logRef kind name =
  LogRef
    { logRefKind = kind
    , logRefName = name
    , logRefCardCode = Nothing
    , logRefCardId = Nothing
    , logRefEntityId = Nothing
    , logRefFaceDown = False
    }

-- | One piece of an entry's sentence. An i18n part's variables are parts themselves, so a
-- translated sentence still carries its card chips.
data LogPart
  = LogText Text
  | LogI18n Text (Map Text LogPart)
  | LogRefPart LogRef
  | LogNumber Int
  | -- | drawn @+2@ / @-1@
    LogDelta Int
  | -- | a stat or sphere icon by name: @"thwart"@, @"threat"@, @"energy"@
    LogIcon Text
  | -- | joined by the locale's list rule
    LogList [LogPart]
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | What sort of thing happened, which sets how the entry is drawn.
data LogKind
  = -- | a heading: "Round 2", "Villain phase"
    Structure
  | -- | a player chose to do something
    Action
  | -- | an engine consequence: damage, progress, a reveal
    Mechanic
  | -- | a contest decided by numbers: an attack, a scheme
    Test
  | -- | true but subordinate: a cost, why a number came out as it did
    Notice
  | -- | something a player typed
    Chat
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Who may read an entry. A player's hand is hidden from the others at a game with
-- friends, so what was drawn is told to that player, and a count to everyone else.
data LogAudience
  = Everyone
  | -- | whoever sees this player's hand
    OnlyPlayer IdentityId
  | -- | whoever does not
    ExceptPlayer IdentityId
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Whether it went well or badly for the players, so the client can colour it.
data LogTone = Good | Bad
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Entries sharing a group id are drawn as one block. Rows stay flat and append-only;
-- grouping is the client's job. An id is a path: @a/b@ is block @b@ inside block @a@.
data LogGroupRole = GroupHeader | GroupMember | GroupSummary
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

data LogGroup = LogGroup
  { logGroupId :: Text
  , logGroupRole :: LogGroupRole
  }
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON LogGroup where
  toJSON = genericToJSON (aesonOptions $ Just "logGroup")

instance FromJSON LogGroup where
  parseJSON = genericParseJSON (aesonOptions $ Just "logGroup")

-- | Where in the game an entry happened, for a reader scrolling back.
data LogContext = LogContext
  { logContextRound :: Int
  , logContextPhase :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON LogContext where
  toJSON = genericToJSON (aesonOptions $ Just "logContext")

instance FromJSON LogContext where
  parseJSON = genericParseJSON (aesonOptions $ Just "logContext")

-- | One entry, possibly holding others beneath it.
--
-- 'logEntrySeq' and 'logEntryStep' are stamped when the entry is stored, not by whoever
-- builds it. The step is the game's step just before the answer that wrote it, so
-- undoing to it puts the game back to just before the entry.
data LogEntry = LogEntry
  { logEntrySeq :: Int
  , logEntryStep :: Maybe Int
  , logEntryKind :: LogKind
  , logEntryBody :: [LogPart]
  , logEntrySource :: Maybe LogRef
  -- ^ what caused it, as data rather than in the sentence
  , logEntryChildren :: [LogEntry]
  , logEntryAudience :: LogAudience
  , logEntryContext :: Maybe LogContext
  , logEntryTone :: Maybe LogTone
  , logEntryGroup :: Maybe LogGroup
  }
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON LogEntry where
  toJSON = genericToJSON (aesonOptions $ Just "logEntry")

instance FromJSON LogEntry where
  parseJSON = genericParseJSON (aesonOptions $ Just "logEntry")

mkLogEntry :: LogKind -> [LogPart] -> LogEntry
mkLogEntry kind body =
  LogEntry
    { logEntrySeq = 0
    , logEntryStep = Nothing
    , logEntryKind = kind
    , logEntryBody = body
    , logEntrySource = Nothing
    , logEntryChildren = []
    , logEntryAudience = Everyone
    , logEntryContext = Nothing
    , logEntryTone = Nothing
    , logEntryGroup = Nothing
    }

-- | Plain text, for traces and tests: refs become names and i18n parts their keys.
logPartToText :: LogPart -> Text
logPartToText = \case
  LogText t -> t
  LogI18n k vars
    | Map.null vars -> "$" <> k
    | otherwise -> "$" <> k <> "(" <> T.intercalate ", " [v <> "=" <> logPartToText p | (v, p) <- Map.toList vars] <> ")"
  LogRefPart r -> r.logRefName
  LogNumber n -> tshow n
  LogDelta n -> (if n < 0 then "" else "+") <> tshow n
  LogIcon t -> "[" <> t <> "]"
  LogList ps -> T.intercalate ", " (map logPartToText ps)

-- | The entry and its children as indented lines.
logEntryToLines :: LogEntry -> [Text]
logEntryToLines = go 0
 where
  go d e =
    (T.replicate d "  " <> "[" <> T.toLower (tshow e.logEntryKind) <> "] " <> foldMap logPartToText e.logEntryBody)
      : concatMap (go (d + 1)) e.logEntryChildren
