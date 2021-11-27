module Marvel.AlterEgo.Attrs
  ( module Marvel.AlterEgo.Attrs
  , module X
  ) where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.Side
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp as X
import Marvel.Id as X
import Marvel.Message
import Marvel.Queue
import Marvel.Source
import Marvel.Stats
import Marvel.Target

alterEgo
  :: (AlterEgoAttrs -> a)
  -> CardDef
  -> HP GameValue
  -> HandSize
  -> Rec
  -> [CardDef]
  -> CardBuilder IdentityId a
alterEgo f cardDef hp hSize recovery obligations = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \ident -> f $ AlterEgoAttrs
    { alterEgoIdentityId = ident
    , alterEgoBaseHandSize = hSize
    , alterEgoBaseRecovery = recovery
    , alterEgoHeroForms = [A]
    , alterEgoStartingHP = hp
    , alterEgoCardDef = cardDef
    , alterEgoObligations = obligations
    }
  }

class IsAlterEgo a

type AlterEgoCard a = CardBuilder IdentityId a

data AlterEgoAttrs = AlterEgoAttrs
  { alterEgoIdentityId :: IdentityId
  , alterEgoBaseHandSize :: HandSize
  , alterEgoBaseRecovery :: Rec
  , alterEgoHeroForms :: [Side]
  , alterEgoStartingHP :: HP GameValue
  , alterEgoCardDef :: CardDef
  , alterEgoObligations :: [CardDef]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasCardCode AlterEgoAttrs where
  toCardCode = toCardCode . alterEgoCardDef

instance IsSource AlterEgoAttrs where
  toSource = IdentitySource . alterEgoIdentityId

instance IsTarget AlterEgoAttrs where
  toTarget = IdentityTarget . alterEgoIdentityId

instance HasHandSize AlterEgoAttrs where
  handSize = alterEgoBaseHandSize

instance HasStartingHP AlterEgoAttrs where
  startingHP = alterEgoStartingHP

instance Entity AlterEgoAttrs where
  type EntityId AlterEgoAttrs = IdentityId
  type EntityAttrs AlterEgoAttrs = AlterEgoAttrs
  toId = toId . toAttrs
  toAttrs = id

instance RunMessage AlterEgoAttrs where
  runMessage msg a = case msg of
    IdentityMessage ident (SideMessage msg') | ident == alterEgoIdentityId a ->
      case msg' of
        Recovered -> do
          push
            (IdentityMessage ident
            $ IdentityHealed
            . unRec
            $ alterEgoBaseRecovery a
            )
          pure a
        _ -> pure a
    _ -> pure a
