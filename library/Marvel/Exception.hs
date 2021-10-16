module Marvel.Exception where

import Marvel.Prelude

import Data.Text qualified as T
import Marvel.Card.Code

data MissingCardCode = MissingCardCode String CardCode
  deriving stock Show

instance Exception MissingCardCode where
  displayException (MissingCardCode fName code) =
    fName <> ": Failed to lookup card with card code " <> show code

data NoPlayers = NoPlayers
  deriving stock Show

instance Exception NoPlayers where
  displayException NoPlayers = "Tried to start a game without any players"

data NoChoices = NoChoices
  deriving stock Show

instance Exception NoChoices where
  displayException NoChoices = "Tried to ask a question with no choices"

newtype UnhandledMessage = UnhandledMessage Text
  deriving stock Show

instance Exception UnhandledMessage where
  displayException (UnhandledMessage s) = T.unpack s
