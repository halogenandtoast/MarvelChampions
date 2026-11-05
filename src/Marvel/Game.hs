{-# OPTIONS_GHC -Wno-orphans #-}

-- Preserve the public API and the instances promised by Marvel.Game.hs-boot.
module Marvel.Game (module X) where

import Marvel.Ability.Types (HasAbilities (..))
import Marvel.Entity (field)
import Marvel.Game.App as X
import Marvel.Game.Base as X
import Marvel.Game.Class as X
import Marvel.Game.Query as X
import Marvel.Game.Runner as X
import Marvel.Game.Select as X
import Marvel.Game.Setup as X
import Marvel.Identity.Types (PlayerIdentity)
import Marvel.Prelude
import Marvel.Projection (Projection (..))

instance (HasGame m) => Projection m PlayerIdentity where
  project fld ident = do
    pIdentity <- getIdentity ident
    pure $ field fld pIdentity

instance HasAbilities Game where
  getAbilities = gameAbilities
