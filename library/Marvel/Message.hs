module Marvel.Message where

import Marvel.Prelude

import Marvel.Card.Code
import {-# SOURCE #-} Marvel.Game
import Marvel.Identity
import Marvel.Player

newtype Sorted a = Sorted { unSorted :: [a] }
  deriving newtype Show

newtype Unsorted a = Unsorted { unUnsorted :: [a] }
  deriving newtype Show

data Message
  = ChoosePlayerOrder IdentityId (Unsorted Player) (Sorted Player)
  | StartGame
  | Ask IdentityId Question
  deriving stock Show

newtype Question = ChooseOne [Choice]
  deriving stock Show

data Choice = CardLabel CardCode [Message]
  deriving stock Show

choiceMessages :: Choice -> [Message]
choiceMessages = \case
  CardLabel _ msgs -> msgs

class RunMessage a where
  runMessage :: MonadGame env m => a -> Message -> m a
