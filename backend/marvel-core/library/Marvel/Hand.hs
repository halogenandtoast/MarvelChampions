module Marvel.Hand where

import Marvel.Prelude

import GHC.Generics
import Marvel.Card.PlayerCard

newtype HandSize = HandSize { unHandSize :: Natural }
  deriving newtype (Show, Eq, ToJSON, FromJSON)

newtype Hand = Hand { unHand :: [PlayerCard] }
  deriving newtype (Show, Eq, ToJSON, FromJSON)

class HasHandSize a where
  handSize :: a -> HandSize

class HasHandSize' f where
  handSize' :: f a -> HandSize

instance HasHandSize' f => HasHandSize' (M1 i c f) where
  handSize' = handSize' . unM1

instance (HasHandSize' l, HasHandSize' r) => HasHandSize' (l :+: r) where
  handSize' = \case
    L1 x -> handSize' x
    R1 x -> handSize' x

instance HasHandSize c => HasHandSize' (K1 i c) where
  handSize' = handSize . unK1

genericHasHandSize :: (HasHandSize' (Rep a), Generic a) => a -> HandSize
genericHasHandSize = handSize' . from
