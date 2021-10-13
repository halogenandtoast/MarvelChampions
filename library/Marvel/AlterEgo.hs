module Marvel.AlterEgo where

import Marvel.Prelude

import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Hp
import Marvel.Identity.Attrs

data AlterEgo = PeterParker' PeterParker | CarolDanvers' CarolDanvers
  deriving stock (Show, Eq, Generic)

allAlterEgos :: HashMap CardCode (IdentityId -> AlterEgo)
allAlterEgos = fromList
  [ ("01001a", PeterParker' . peterParker)
  , ("01010a", CarolDanvers' . carolDanvers)
  ]

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


instance HasStartingHP AlterEgo where
  startingHP = defaultHasStartingHP

instance HasIdentityAttrs AlterEgo where
  toIdentityAttrs = genericToIdentityAttrs

peterParker :: IdentityId -> PeterParker
peterParker =
  alterEgo PeterParker Cards.peterParker (HP 10) (HandSize 6) (Rec 3)

newtype PeterParker = PeterParker AlterEgoAttrs
  deriving newtype (Show, Eq, HasStartingHP, HasIdentityAttrs)

carolDanvers :: IdentityId -> CarolDanvers
carolDanvers =
  alterEgo CarolDanvers Cards.carolDanvers (HP 12) (HandSize 6) (Rec 4)

newtype CarolDanvers = CarolDanvers AlterEgoAttrs
  deriving newtype (Show, Eq, HasStartingHP, HasIdentityAttrs)

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

