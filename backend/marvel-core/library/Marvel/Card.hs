{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.Card (
  module X,
) where

import Marvel.Prelude

import Marvel.Card.Builder as X
import Marvel.Card.Code as X
import Marvel.Card.Def as X
import Marvel.Card.EncounterCard as X
import Marvel.Card.Id as X
import Marvel.Card.PlayerCard as X
import Marvel.Card.Side as X
import Marvel.Card.Types as X

import Marvel.Resource

instance HasResources Card where
  resourcesFor (EncounterCard _) _ = pure []
  resourcesFor (PlayerCard x) mc = resourcesFor x mc
