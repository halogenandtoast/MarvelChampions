module Marvel.Exception where

import Marvel.Prelude

import Control.Exception
import Data.Text qualified as T
import Marvel.Card.Code

data MissingCardCode = MissingCardCode String CardCode
  deriving stock (Show)

instance Exception MissingCardCode where
  displayException (MissingCardCode fName code) =
    fName <> ": Failed to lookup card with card code " <> show code

newtype ChangeFormFailure = ChangeFormFailure Text
  deriving stock (Show)

instance Exception ChangeFormFailure where
  displayException (ChangeFormFailure s) = T.unpack s

data NoPlayers = NoPlayers
  deriving stock (Show)

instance Exception NoPlayers where
  displayException NoPlayers = "Tried to start a game without any players"

data NoChoices = NoChoices
  deriving stock (Show)

instance Exception NoChoices where
  displayException NoChoices = "Tried to ask a question with no choices"

newtype UnhandledMessage = UnhandledMessage Text
  deriving stock (Show)

instance Exception UnhandledMessage where
  displayException (UnhandledMessage s) = T.unpack s

-- | An answer for a player who was not asked anything.
data NoQuestion = NoQuestion
  deriving stock (Show)

instance Exception NoQuestion

-- | An answer naming a choice the question does not have.
data InvalidChoice = InvalidChoice
  deriving stock (Show)

instance Exception InvalidChoice
