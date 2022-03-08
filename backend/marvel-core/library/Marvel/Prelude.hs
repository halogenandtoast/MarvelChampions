{-# LANGUAGE QuantifiedConstraints #-}
module Marvel.Prelude
  ( module Marvel.Prelude
  , module X
  ) where

import Control.Lens as X
  ( Lens'
  , Traversal'
  , _Just
  , at
  , each
  , ix
  , lens
  , preview
  , set
  , traverseOf
  , view
  , (%~)
  , (+~)
  , (-~)
  , (.~)
  , (<>~)
  , (?~)
  , (^.)
  )
import Control.Lens.TH as X
import Control.Monad.Catch as X (MonadCatch, MonadThrow, handleAll, throwM)
import Control.Monad.Extra as X (concatMapM)
import Control.Monad.Random as X (MonadRandom, Random, getRandom)
import Data.Aeson as X hiding (Key)
import Data.Traversable as X (for)
import Data.UUID as X (UUID)
import Data.Vector as X (Vector)
import GHC.Natural as X (minusNaturalMaybe)
import Relude as X hiding (One)
import Relude.Extra.Map as X

import Data.Aeson.Casing (camelCase)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Text
import Data.Char qualified as C
import Data.List qualified as List
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder
import Language.Haskell.TH hiding (location)
import Text.Show qualified as S

subtractNatural :: Natural -> Natural -> Natural
subtractNatural a b = fromMaybe 0 (minusNaturalMaybe b a)

tshow :: Show a => a -> T.Text
tshow = T.pack . show

count :: (a -> Bool) -> [a] -> Natural
count _ [] = 0
count f (x : xs) = let n = if f x then 1 else 0 in n + count f xs

suffixedNamer :: FieldNamer
suffixedNamer _ _ n = case dropWhile C.isLower (nameBase n) of
  x : xs -> [TopName (mkName ((C.toLower x : xs) ++ "L"))]
  _ -> []

suffixedWithNamer :: String -> FieldNamer
suffixedWithNamer str _ _ n = case drop (length str) (nameBase n) of
  x : xs -> [TopName (mkName ((C.toLower x : xs) ++ "L"))]
  _ -> []

suffixedWithFields :: String -> LensRules
suffixedWithFields suffix =
  defaultFieldRules & lensField .~ suffixedWithNamer suffix

suffixedFields :: LensRules
suffixedFields = defaultFieldRules & lensField .~ suffixedNamer

aesonOptions :: Maybe String -> Options
aesonOptions ms = defaultOptions { fieldLabelModifier = camelCase . drop len }
  where len = maybe 0 length ms

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

class (forall a b. Coercible a b => Coercible (f a) (f b)) => CoerceRole f
instance (forall a b. Coercible a b => Coercible (f a) (f b)) => CoerceRole f

data With a b = With a b

instance (Eq a, Eq b) => Eq (With a b) where
  With a1 b1 == With a2 b2 = a1 == a2 && b1 == b2

instance (ToJSON a, ToJSON b) => ToJSON (a `With` b) where
  toJSON (a `With` b) = case (toJSON a, toJSON b) of
    (Object o, Object m) -> Object $ KeyMap.union m o
    (a', b') -> metadataError a' b'
   where
    metadataError a' b' =
      error
        . show
        . TL.unpack
        . toLazyText
        $ "With failed to serialize to object: "
        <> "\nattrs: "
        <> encodeToTextBuilder a'
        <> "\nmetadata: "
        <> encodeToTextBuilder b'

instance (FromJSON a, FromJSON b) => FromJSON (a `With` b) where
  parseJSON = withObject "With"
    $ \o -> With <$> parseJSON (Object o) <*> parseJSON (Object o)

instance (S.Show a, S.Show b) => S.Show (a `With` b) where
  show (With a b) = show a <> " WITH " <> show b

removeEach :: Eq a => [a] -> [(a, [a])]
removeEach xs = zip xs (map (`List.delete` xs) xs)

toFst :: (a -> b) -> a -> (b, a)
toFst f a = (f a, a)
