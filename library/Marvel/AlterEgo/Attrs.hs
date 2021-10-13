module Marvel.AlterEgo.Attrs
  ( module Marvel.AlterEgo.Attrs
  , module X
  ) where

import Marvel.Prelude

import Marvel.Card.Def
import Marvel.Hp as X
import Marvel.Identity.Attrs as X

alterEgo
  :: (AlterEgoAttrs -> a) -> CardDef -> HP -> HandSize -> Rec -> IdentityId -> a
alterEgo f cardDef hp handSize recovery identityAttrsId = f $ AlterEgoAttrs
  { alterEgoIdentityAttrs = IdentityAttrs { .. }
  , alterEgoBaseHandSize = handSize
  , alterEgoBaseRecovery = recovery
  }
 where
  identityAttrsCardDef = cardDef
  identityAttrsStartingHP = hp
  identityAttrsCurrentHP = hp
  identityAttrsMaxHP = hp

class IsAlterEgo a

newtype HandSize = HandSize Int
  deriving newtype (Show, Eq)

newtype Rec = Rec Int
  deriving newtype (Show, Eq)

data AlterEgoAttrs = AlterEgoAttrs
  { alterEgoIdentityAttrs :: IdentityAttrs
  , alterEgoBaseHandSize :: HandSize
  , alterEgoBaseRecovery :: Rec
  }
  deriving stock (Show, Eq)

instance HasStartingHP AlterEgoAttrs where
  startingHP = startingHP . toIdentityAttrs

instance HasIdentityAttrs AlterEgoAttrs where
  toIdentityAttrs = alterEgoIdentityAttrs
