module Marvel.Question where

import Marvel.Prelude

import Marvel.Ref (Target)

data Question

instance Show Question
instance Eq Question
instance ToJSON Question
instance FromJSON Question

-- | Whether the question asks to resolve the When Revealed of this.
asksToResolveWhenRevealed :: Target -> Question -> Bool

data Choice

instance Show Choice
instance Eq Choice
instance ToJSON Choice
instance FromJSON Choice
