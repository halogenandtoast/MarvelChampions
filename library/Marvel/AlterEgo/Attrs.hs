module Marvel.AlterEgo.Attrs
  ( module Marvel.AlterEgo.Attrs
  , module X
  ) where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Def
import Marvel.Message
import Marvel.Hp as X
import Marvel.Identity.Attrs as X

alterEgo
  :: (AlterEgoAttrs -> a)
  -> CardDef
  -> HP
  -> HandSize
  -> Rec
  -> CardBuilder IdentityId a
alterEgo f cardDef hp handSize recovery = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \identityAttrsId -> f $ AlterEgoAttrs
    { alterEgoIdentityAttrs = IdentityAttrs { .. }
    , alterEgoBaseHandSize = handSize
    , alterEgoBaseRecovery = recovery
    }
  }
 where
  identityAttrsCardDef = cardDef
  identityAttrsStartingHP = hp
  identityAttrsCurrentHP = hp
  identityAttrsMaxHP = hp
  identityAttrsDeck = []

class IsAlterEgo a

type AlterEgoCard a = CardBuilder IdentityId a

newtype HandSize = HandSize Int
  deriving newtype (Show, Eq, ToJSON, FromJSON)

newtype Rec = Rec Int
  deriving newtype (Show, Eq, ToJSON, FromJSON)

data AlterEgoAttrs = AlterEgoAttrs
  { alterEgoIdentityAttrs :: IdentityAttrs
  , alterEgoBaseHandSize :: HandSize
  , alterEgoBaseRecovery :: Rec
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance RunMessage AlterEgoAttrs where
  runMessage msg x = do
    identityAttrs' <- runMessage msg (alterEgoIdentityAttrs x)
    pure $ x { alterEgoIdentityAttrs = identityAttrs' }

instance HasStartingHP AlterEgoAttrs where
  startingHP = startingHP . toIdentityAttrs

instance HasIdentityAttrs AlterEgoAttrs where
  toIdentityAttrs = alterEgoIdentityAttrs
