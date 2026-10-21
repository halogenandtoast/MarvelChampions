{-# OPTIONS_GHC -Wno-partial-fields #-}

module Marvel.Frontend (module Marvel.Frontend) where

import Marvel.Card.Id
import Marvel.Log.Entry
import Marvel.Prelude

-- | What the game tells its clients besides the game itself.
data FrontendMessage
  = -- | an entry for the game log, stamped and stored before it goes out
    Logged {entry :: LogEntry}
  | -- | cards shuffled back into a deck, which may be drawn straight out again
    ShuffleIntoDeck {cardIds :: [CardId]}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

class (Monad m) => Frontend m where
  send :: FrontendMessage -> m ()
