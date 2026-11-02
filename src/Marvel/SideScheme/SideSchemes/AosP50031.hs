{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.SideScheme.SideSchemes.AosP50031 where

import Marvel.Prelude
import Marvel.AgentsOfShield.Helpers
import Marvel.Id (MinionId)
import Marvel.Trait qualified as T
import Marvel.Query
import Marvel.Matchers
import Marvel.Game.Source
import Marvel.Environment.Cards qualified as Environments
import Marvel.Card
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.GameValue

aosP50031 :: SideSchemeCard AosP50031
aosP50031 = sideScheme AosP50031 Cards.aosP50031 (PerPlayer 3)

newtype AosP50031 = AosP50031 (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage AosP50031 where
  runMessage msg e@(AosP50031 a) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId a -> do
      first <- selectJust FirstPlayer
      searchEverywhereAndPutIntoPlayEncounterCard first (CardIs Environments.aosP50032)
      pure e
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId a -> do
      mids <- selectList (MinionWithTrait T.Controlled <> MinionCanBeDiscarded)
      traverse_ removeFromPlay mids
      cardSpecific a "rewardDiscarded" mids
      pure e
    CardSpecific (isTarget a -> True) "rewardDiscarded" (fromJSON -> Success (mids :: [MinionId])) -> do
      remaining <- selectList AnyMinion
      cardSpecific a "placeSupportCounters" (fromIntegral (length $ filter (`notElem` remaining) mids) :: Natural)
      pure e
    CardSpecific (isTarget a -> True) "placeSupportCounters" (fromJSON -> Success (n :: Natural)) -> do
      when (n > 0) do
        supports <- selectList (SupportControlledBy AnyIdentity)
        first <- selectJust FirstPlayer
        chooseEffect first [targeting sid (placeCounters sid 1 >> cardSpecific a "placeSupportCounters" (n-1)) | sid <- supports]
      pure e
    _ -> AosP50031 <$> runMessage msg a
