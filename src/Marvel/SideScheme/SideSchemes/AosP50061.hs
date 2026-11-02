{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.SideScheme.SideSchemes.AosP50061 where

import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Card
import Marvel.Cost
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.GameValue

aosP50061 :: SideSchemeCard AosP50061
aosP50061 = sideScheme AosP50061 Cards.aosP50061 (Static 5)

newtype AosP50061 = AosP50061 (Attrs SideScheme)
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsSideScheme AosP50061 where
  sideSchemeAbilities (AosP50061 a) = [windowAbility a 1 (W.IdentityDamaged W.After $ IdentityWithTitle "Nick Fury") ForcedResponse NoCost (runAbility a 1)]

instance RunMessage AosP50061 where
  runMessage msg e@(AosP50061 a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      orion <- selectList (MinionIs Minions.aosP50060)
      if null orion then do
        nick <- selectOne (IdentityWithTitle "Nick Fury")
        for_ nick $ \you -> searchEverywhereAndPutIntoPlayEncounterCard you (CardIs Minions.aosP50060)
      else traverse_ (`giveStatus` ToughStatus) orion
      pure e
    _ -> AosP50061 <$> runMessage msg a
