module Marvel.AlterEgo.Attrs
  ( module Marvel.AlterEgo.Attrs
  , module X
  ) where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Def
import Marvel.Card.Side
import Marvel.Entity
import Marvel.Hp as X
import Marvel.Id as X
import Marvel.Identity.Attrs as X
import Marvel.Message
import Marvel.Question

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
    { alterEgoIdentityAttrs = defaultAttrs ident cardDef hp
    , alterEgoBaseHandSize = handSize
    , alterEgoBaseRecovery = recovery
    , alterEgoHeroForms = [A]
    }
  }

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
  , alterEgoHeroForms :: [Side]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Entity AlterEgoAttrs where
  type EntityId AlterEgoAttrs = IdentityId
  type EntityAttrs AlterEgoAttrs = IdentityAttrs
  toId = toId . toAttrs
  toAttrs = view identityAttrsL

instance RunMessage AlterEgoAttrs where
  runMessage msg x = case msg of
    IdentityMessage ident ChooseOtherForm | ident == toId x -> do
      chooseOrRunOne ident $ map ChangeToForm (alterEgoHeroForms x)
      pure x
    other -> do
      identityAttrs' <- runMessage other (alterEgoIdentityAttrs x)
      pure $ x { alterEgoIdentityAttrs = identityAttrs' }

instance HasStartingHP AlterEgoAttrs where
  startingHP = startingHP . view identityAttrsL

instance HasIdentityAttrs AlterEgoAttrs where
  identityAttrsL =
    lens alterEgoIdentityAttrs \m x -> m { alterEgoIdentityAttrs = x }
