module Marvel.Card.Code where

import Marvel.Prelude

import Data.Text qualified as T
import GHC.Generics
import Marvel.Card.Side

newtype CardCode = CardCode { unCardCode :: Text }
  deriving newtype (IsString, Show, Eq, Ord, Hashable, ToJSON, FromJSON, FromJSONKey)

toBaseCardCode :: CardCode -> CardCode
toBaseCardCode cardCode = case T.unsnoc $ unCardCode cardCode of
  Nothing -> cardCode
  Just (base, 'a') -> CardCode base
  Just (base, 'b') -> CardCode base
  Just (base, 'c') -> CardCode base
  Just _ -> cardCode

flipToSide :: Side -> CardCode -> CardCode
flipToSide side cardCode = CardCode
  $ T.snoc (unCardCode $ toBaseCardCode cardCode) (toCardCodePiece side)

toAlterEgoCardCode :: CardCode -> CardCode
toAlterEgoCardCode = flipToSide B

toHeroCardCode :: CardCode -> CardCode
toHeroCardCode = flipToSide A

class HasCardCode a where
  toCardCode :: a -> CardCode

class HasCardCode' f where
  toCardCode' :: f p -> CardCode

genericToCardCode :: (Generic a, HasCardCode' (Rep a)) => a -> CardCode
genericToCardCode = toCardCode' . from

instance HasCardCode' f => HasCardCode' (M1 i c f) where
  toCardCode' = toCardCode' . unM1

instance (HasCardCode' l, HasCardCode' r) => HasCardCode' (l :+: r) where
  toCardCode' = \case
    L1 l -> toCardCode' l
    R1 r -> toCardCode' r

instance HasCardCode c => HasCardCode' (K1 i c) where
  toCardCode' = toCardCode . unK1
