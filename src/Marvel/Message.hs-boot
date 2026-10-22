module Marvel.Message where

import Marvel.Prelude

data Message
class RunMessage a
simultaneousMessages :: Message -> Maybe [Message]
simultaneously :: [Message] -> Message
instance ToJSON Message
instance FromJSON Message
instance Show Message
instance Eq Message
