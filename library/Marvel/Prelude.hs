module Marvel.Prelude
  ( module Marvel.Prelude
  , module X
  ) where

import Control.Lens as X
  (Lens', at, each, lens, set, traverseOf, view, (%~), (.~), (<>~), (?~))
import Control.Monad.Catch as X (MonadCatch, MonadThrow, handleAll, throwM)
import Control.Monad.Random as X (MonadRandom, Random, getRandom)
import Data.Aeson as X (FromJSON(..), FromJSONKey, ToJSON(..))
import Data.UUID as X (UUID)
import Data.Vector as X (Vector)
import Relude as X
import Relude.Extra.Map as X

import Control.Lens.TH as X
import Data.Char qualified as C
import Data.Text qualified as T
import Language.Haskell.TH hiding (location)

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
