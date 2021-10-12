module Marvel.AlterEgo where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Hp
import Marvel.Identity.Attrs

data AlterEgo = PeterParker' PeterParker | CarolDanvers' CarolDanvers
  deriving stock (Show, Eq, Generic)

allAlterEgos :: HashMap CardCode (IdentityId -> AlterEgo)
allAlterEgos = fromList
  [ ("01001", PeterParker' . peterParker)
  , ("01010", CarolDanvers' . carolDanvers)
  ]

instance HasStartingHP AlterEgo where
  startingHP = defaultHasStartingHP

instance HasIdentityAttrs AlterEgo where
  toIdentityAttrs = genericToIdentityAttrs

peterParker :: IdentityId -> PeterParker
peterParker ident =
  PeterParker $ AlterEgoAttrs $ IdentityAttrs ident "01001" $ HP 10

newtype PeterParker = PeterParker AlterEgoAttrs
  deriving newtype (Show, Eq, HasStartingHP, HasIdentityAttrs)

carolDanvers :: IdentityId -> CarolDanvers
carolDanvers ident =
  CarolDanvers $ AlterEgoAttrs $ IdentityAttrs ident "01010" $ HP 12

newtype CarolDanvers = CarolDanvers AlterEgoAttrs
  deriving newtype (Show, Eq, HasStartingHP, HasIdentityAttrs)

newtype AlterEgoAttrs = AlterEgoAttrs IdentityAttrs
  deriving newtype (Show, Eq, HasStartingHP, HasIdentityAttrs)
