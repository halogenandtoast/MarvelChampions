module Marvel.Card.Code where

import Marvel.Prelude

newtype CardCode = CardCode Text
  deriving newtype (IsString, Show, Eq, Hashable)

class HasCardCode a where
  toCardCode :: a -> CardCode
