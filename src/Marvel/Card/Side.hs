module Marvel.Card.Side where

import Marvel.Prelude

data Side = A | B | C
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable, ToJSONKey, FromJSONKey)

toCardCodePiece :: Side -> Char
toCardCodePiece = \case
  A -> 'a'
  B -> 'b'
  C -> 'c'
