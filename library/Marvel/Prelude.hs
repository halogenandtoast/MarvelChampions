module Marvel.Prelude
  ( module Marvel.Prelude
  , module X
  ) where

import Control.Lens as X (Lens', at, lens, traverseOf, view, (%~), (.~), (?~))
import Control.Monad.Random as X (MonadRandom, Random, getRandom)
import Data.Aeson as X (FromJSON(..), ToJSON(..))
import Data.UUID as X (UUID)
import Data.Vector as X (Vector)
import Relude as X
import Relude.Extra.Map as X

import Data.Text qualified as T

tshow :: Show a => a -> T.Text
tshow = T.pack . show
