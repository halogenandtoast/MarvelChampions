{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Model (module Model) where

import AppPrelude
import Data.ByteString.Char8 qualified as BS8
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Database.Persist.Postgresql.JSON ()
import Database.Persist.Sql
import Database.Persist.TH
import Yesod.Core (PathPiece (..))

instance PersistField UUID where
  toPersistValue = PersistLiteral_ Escaped . BS8.pack . UUID.toString
  fromPersistValue (PersistLiteral_ _ t) = maybe (Left "Invalid UUID") Right $ UUID.fromString $ BS8.unpack t
  fromPersistValue _ = Left "Not PersistDBSpecific"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

instance PathPiece UUID where
  toPathPiece = UUID.toText
  fromPathPiece = UUID.fromText

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User sql=users
  username Text
  email Text
  passwordDigest Text
  admin Bool default=false
  -- | sees the beta scenarios when making a game
  beta Bool default=false
  -- | what "allow spectators" starts as when making a game
  allowSpectatorsByDefault Bool default=false
  -- | made by "play as guest": no password until they sign up, which keeps this row
  guest Bool default=false
  createdAt UTCTime
  UniqueUsername username
  UniqueEmail email
  deriving Show

MarvelGame sql=marvel_games
  Id UUID default=gen_random_uuid()
  userId UserId OnDeleteCascade
  name Text
  playerName Text
  currentData Value
  currentQueue Value
  randomGen Text
  step Int
  -- | what reset deals again: the scenario as set up, and the decks
  scenario Value
  deck Value Maybe
  -- | whether anyone may watch, not only the owner and admins; chosen when the game is made
  allowSpectators Bool default=false
  -- | played with friends, a seat each, rather than every seat being the owner's
  withFriends Bool default=false
  -- | a game with friends is dealt once all of its seats are taken; until then its
  -- current data is null
  seats Int default=1
  -- | the campaign it is a scenario of, as it was dealt: reset deals it again so
  campaign Value Maybe
  -- | The owning campaign; deleting it removes all its scenario games and their logs.
  campaignId MarvelCampaignId Maybe OnDeleteCascade
  createdAt UTCTime
  updatedAt UTCTime
  deriving Show

-- | A campaign ("Marvel.Campaign"): its log as of the last game won, and the game of it
-- being played, if one is. Who plays it, with the decks they began it with, are its
-- seats; each game's decks are those and the cards the log adds.
MarvelCampaign sql=marvel_campaigns
  Id UUID default=gen_random_uuid()
  userId UserId OnDeleteCascade
  name Text
  campaign Value
  -- | the decks of a campaign made before it had seats, its owner playing each
  decks Value
  -- | each scenario's difficulty: standard or expert
  difficulty Value
  currentGame MarvelGameId Maybe OnDeleteSetNull
  -- | played with friends, a seat each, its games dealt to their seats
  withFriends Bool default=false
  seats Int default=1
  createdAt UTCTime
  updatedAt UTCTime
  deriving Show

-- | Who sits where in a campaign, and the deck they began it with: seat n plays the nth
-- player of each of its games.
MarvelCampaignSeat sql=marvel_campaign_seats
  campaignId MarvelCampaignId OnDeleteCascade
  position Int
  userId UserId OnDeleteCascade
  deck Value
  UniqueCampaignSeat campaignId position
  deriving Show

-- | Who sits where at a game with friends, and the deck they brought. Seats are dealt
-- in position order, so position n plays the nth player. The owner may take several
-- seats; whoever joins takes one.
MarvelSeat sql=marvel_seats
  gameId MarvelGameId OnDeleteCascade
  position Int
  userId UserId OnDeleteCascade
  deck Value Maybe
  UniqueSeat gameId position
  deriving Show

-- | Enough to step a game back one answer: a patch from the game after the answer to
-- the game before it, and the queue and random generator from before it.
MarvelStep sql=marvel_steps
  gameId MarvelGameId OnDeleteCascade
  step Int
  patchDown Value
  queue Value
  randomGen Text
  round Int
  phase Text
  createdAt UTCTime
  UniqueGameStep gameId step
  deriving Show

-- | One entry of a game's log, written under the step the game was on before the answer
-- that wrote it, so undoing to that step takes it back out. Entries written while the
-- game was dealt sit before step 0, where no undo reaches. Rows are only ever added,
-- and deleted by step; chat survives an undo, moved to just before the step undone to.
MarvelLogEntry sql=marvel_log_entries
  gameId MarvelGameId OnDeleteCascade
  step Int
  seq Int
  kind Text
  payload Value
  createdAt UTCTime
  UniqueLogSeq gameId seq
  deriving Show

-- | An imported deck list: a hero and the cards with them, by MarvelCDB code.
MarvelDeck sql=marvel_decks
  userId UserId OnDeleteCascade
  name Text
  description Text
  hero Text
  aspects [Text]
  source Text
  sourceId Text Maybe
  sourceUrl Text Maybe
  createdAt UTCTime
  updatedAt UTCTime
  usedAt UTCTime Maybe
  -- ^ when a game was last dealt from it
  deriving Show

MarvelDeckSlot sql=marvel_deck_slots
  deckId MarvelDeckId OnDeleteCascade
  position Int
  cardCode Text
  quantity Int
  deriving Show
|]
