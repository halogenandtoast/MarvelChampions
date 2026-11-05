module Marvel.Game (module X) where

import {-# SOURCE #-} Marvel.Game.Base as X (Game)
import Marvel.Game.Class as X
import {-# SOURCE #-} Marvel.Game.Query as X
import {-# SOURCE #-} Marvel.Game.Runner as X
import {-# SOURCE #-} Marvel.Game.Select as X

import Marvel.Ability.Types (HasAbilities)
import Marvel.Identity.Types (PlayerIdentity)
import Marvel.Projection (Projection)

instance (HasGame m) => Projection m PlayerIdentity
instance HasAbilities Game
