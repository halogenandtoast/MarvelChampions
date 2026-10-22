module Marvel.Card.Builder where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def

-- | A card's entity, built from the def of the printing in play: a reprint shows its
-- own code, art and set.
data CardBuilder ident a = CardBuilder
  { cbCardDef :: CardDef
  , cbCardBuilder :: CardDef -> ident -> a
  }

instance Functor (CardBuilder ident) where
  fmap f builder = builder {cbCardBuilder = \def -> f . cbCardBuilder builder def}

instance HasCardCode (CardBuilder ident a) where
  toCardCode = toCardCode . cbCardDef

buildCard :: CardBuilder ident a -> ident -> a
buildCard builder = cbCardBuilder builder (cbCardDef builder)

-- | The same card under another printing's def, as a reprint in another encounter set.
reprintAs :: CardDef -> CardBuilder ident a -> CardBuilder ident a
reprintAs def builder = builder {cbCardDef = def}

-- | The card under each printing its def lists.
printings :: CardBuilder ident a -> [CardBuilder ident a]
printings builder = [reprintAs def builder | (_, def) <- toCardCodePairs (cbCardDef builder)]
