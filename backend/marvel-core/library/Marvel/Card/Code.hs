module Marvel.Card.Code where

import Marvel.Prelude

import qualified Data.Text as T
import Marvel.Card.Side

newtype CardCode = CardCode { unCardCode :: Text }
  deriving newtype (IsString, Show, Eq, Ord, Hashable, ToJSON, FromJSON, FromJSONKey, ToJSONKey)

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
