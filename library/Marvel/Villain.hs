module Marvel.Villain where

import Marvel.Prelude

import Marvel.Card.Code

newtype VillainId = VillainId UUID
  deriving newtype (Show, Eq, Random, Hashable)

data Villain = Rhino' Rhino | Klaw' Klaw
  deriving stock Show

lookupVillain :: CardCode -> VillainId -> Maybe Villain
lookupVillain cardCode villainId = lookup cardCode allVillains <*> pure villainId

allVillains :: HashMap CardCode (VillainId -> Villain)
allVillains = fromList [("01094", Rhino' . rhino)]

newtype Rhino = Rhino VillainAttrs
  deriving newtype Show

rhino :: VillainId -> Rhino
rhino = Rhino . VillainAttrs

newtype Klaw = Klaw VillainAttrs
  deriving newtype Show

newtype VillainAttrs = VillainAttrs { villainId :: VillainId }
  deriving stock Show
