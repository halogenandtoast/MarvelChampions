module Marvel.AlterEgo where

import Marvel.Prelude

import Marvel.AlterEgo.AlterEgos
import Marvel.AlterEgo.Attrs
import Marvel.Card.Code

data AlterEgo
  = PeterParker' PeterParker
  | CarolDanvers' CarolDanvers
  deriving stock (Show, Eq, Generic)

allAlterEgos :: HashMap CardCode (IdentityId -> AlterEgo)
allAlterEgos = fromList
  [ ("01001a", PeterParker' . peterParker)
  , ("01010a", CarolDanvers' . carolDanvers)
  ]

instance HasStartingHP AlterEgo where
  startingHP = defaultHasStartingHP

instance HasIdentityAttrs AlterEgo where
  toIdentityAttrs = genericToIdentityAttrs
