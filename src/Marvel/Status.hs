module Marvel.Status where

import Marvel.Prelude

-- | The status cards a character can be given, one of each type at most.
data StatusCard = StunnedStatus | ConfusedStatus | ToughStatus
  deriving stock (Show, Eq, Generic, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON, Hashable)

-- | The status card's name, as a label the client shows.
statusLabel :: StatusCard -> Text
statusLabel = \case
  StunnedStatus -> "$label.stunnedStatus"
  ConfusedStatus -> "$label.confusedStatus"
  ToughStatus -> "$label.toughStatus"
