module Marvel.AlterEgo.Attrs
  ( module Marvel.AlterEgo.Attrs
  , module X
  ) where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.Side
import Marvel.Hand
import Marvel.Hp as X
import Marvel.Id as X
import Marvel.Source

alterEgo
  :: (AlterEgoAttrs -> a)
  -> CardDef
  -> HP
  -> HandSize
  -> Rec
  -> CardBuilder IdentityId a
alterEgo f cardDef hp handSize recovery = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \ident -> f $ AlterEgoAttrs
    { alterEgoIdentityId = ident
    , alterEgoBaseHandSize = handSize
    , alterEgoBaseRecovery = recovery
    , alterEgoHeroForms = [A]
    , alterEgoStartingHP = hp
    , alterEgoCardDef = cardDef
    }
  }

class IsAlterEgo a

type AlterEgoCard a = CardBuilder IdentityId a

newtype Rec = Rec Int
  deriving newtype (Show, Eq, ToJSON, FromJSON)

data AlterEgoAttrs = AlterEgoAttrs
  { alterEgoIdentityId :: IdentityId
  , alterEgoBaseHandSize :: HandSize
  , alterEgoBaseRecovery :: Rec
  , alterEgoHeroForms :: [Side]
  , alterEgoStartingHP :: HP
  , alterEgoCardDef :: CardDef
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasCardCode AlterEgoAttrs where
  toCardCode = toCardCode . alterEgoCardDef

instance IsSource AlterEgoAttrs where
  toSource = IdentitySource . alterEgoIdentityId

instance HasStartingHP AlterEgoAttrs where
  startingHP = alterEgoStartingHP
