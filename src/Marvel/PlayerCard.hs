module Marvel.PlayerCard
  ( module Marvel.PlayerCard
  , module X
  ) where

import Marvel.Prelude

import Marvel.Ally.Cards
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.PlayerCard as X
import Marvel.Event.Cards
import Marvel.Resource.Cards
import Marvel.Support.Cards
import Marvel.Upgrade.Cards

lookupPlayerCard :: CardCode -> CardDef
lookupPlayerCard code = case lookup code allPlayerCards of
  Just x -> x
  Nothing -> error $ "Missing card " <> show code

allPlayerCards :: HashMap CardCode CardDef
allPlayerCards =
  allAllies <> allEvents <> allSupports <> allUpgrades <> allResources
