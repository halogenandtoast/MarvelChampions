-- | The server's prelude: the engine's is for the engine, this one for handlers.
module AppPrelude (module X) where

import ClassyPrelude as X hiding (Handler, delete, foldlM, on, orElse, (\\))
import Control.Lens as X ((%~), (&), (.~), (?~), (^.))
import Control.Monad.Fail as X (MonadFail (fail))
import Data.Aeson as X hiding (Result (..))
import Data.List as X ((\\))
import Data.List.NonEmpty as X (NonEmpty (..), nonEmpty)
import Safe as X (atMay)
