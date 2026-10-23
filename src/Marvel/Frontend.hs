{-# OPTIONS_GHC -Wno-partial-fields #-}

module Marvel.Frontend (module Marvel.Frontend) where

import Marvel.Card.Code
import Marvel.Card.Id
import Marvel.Log.Entry
import Marvel.Prelude

-- | What the game tells its clients besides the game itself.
data FrontendMessage
  = -- | an entry for the game log, stamped and stored before it goes out
    Logged {entry :: LogEntry}
  | -- | cards shuffled back into a deck, which may be drawn straight out again
    ShuffleIntoDeck {cardIds :: [CardId]}
  | -- | a deck shuffled: @deck-encounter@ or @deck-@ and the player's identity
    Shuffled {pile :: Text}
  | -- | a boost card dealt face down from the encounter deck
    BoostDealt {cardId :: CardId}
  | -- | an encounter card turned face up, revealed or flipped as a boost
    Revealing {cardId :: CardId, code :: CardCode, boost :: Bool}
  | -- | the game as it stands mid-move, so what came before a card dealt into the river is
    -- seen before it
    Board {board :: Value}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

class (Monad m) => Frontend m where
  send :: FrontendMessage -> m ()
