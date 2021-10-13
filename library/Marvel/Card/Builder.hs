module Marvel.Card.Builder where

import Marvel.Prelude

import Marvel.Card.Code

data CardBuilder ident a = CardBuilder
  { cbCardCode :: CardCode
  , cbCardBuilder :: ident -> a
  }

instance Functor (CardBuilder ident) where
  fmap f builder@CardBuilder {..} =
    builder { cbCardBuilder = f . cbCardBuilder }

instance HasCardCode (CardBuilder ident a) where
  toCardCode = cbCardCode
