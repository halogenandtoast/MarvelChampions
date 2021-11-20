module Marvel.Prelude
  ( module Marvel.Prelude
  , module X
  ) where

import Control.Lens as X
  ( Lens'
  , at
  , each
  , ix
  , lens
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
import Data.Aeson as X
import Data.Traversable as X (for)
import Data.UUID as X (UUID)
import Data.Vector as X (Vector)
import Relude as X hiding (One)
import Relude.Extra.Map as X
import GHC.Natural as X (minusNaturalMaybe)

import Data.Aeson.Casing (camelCase)
import Data.Char qualified as C
import Data.Text qualified as T
import Language.Haskell.TH hiding (location)

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

