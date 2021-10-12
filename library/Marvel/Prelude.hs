module Marvel.Prelude
  ( module Marvel.Prelude
  , module X
  ) where

import Control.Monad.Random as X (MonadRandom, Random, getRandom)
import Data.UUID as X (UUID)
import Data.Vector as X (Vector)
import Relude as X
import Relude.Extra.Lens as X
import Relude.Extra.Map as X

import Data.Text qualified as T

tshow :: Show a => a -> T.Text
tshow = T.pack . show
