module Marvel.Encounter.CountsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Trait (Trait (Hydra))
import TestImport

drones :: GameAppT [MinionId]
drones = getsGame $ map toId . filter ((== Minions.drone) . getCardDef) . HashMap.elems . gameMinions

spec :: Spec
spec = describe "encounter cards that count what is in play" do
  it "Drone Factory places 1 threat for each Drone, counted once each player's is in play" do
    tg <- testGameAgainst "01134" ["spider-man", "captain-marvel"]
    let p = player1 tg
    inGame tg do
      card <- revealNow p SideSchemes.droneFactory
      pass
      n <- length <$> drones
      -- its own 2 per player, then 1 for each drone
      sideSchemeThreat (sideSchemeOf card) `shouldReturn` fromIntegral (n + 4)

  it "Legions of Hydra counts Madame Hydra once she is found" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- stackEncounterDeck [Minions.madameHydra]
      card <- revealNow p SideSchemes.legionsOfHydra
      pass
      hydra <- getsGame $ length . filter ((Hydra `elem`) . cdTraits . getCardDef) . HashMap.elems . gameMinions
      hydra `shouldSatisfy` (>= 1)
      -- its starting 3, then 2 for each Hydra enemy
      sideSchemeThreat (sideSchemeOf card) `shouldReturn` fromIntegral (3 + 2 * hydra)

  it "Swarm Attack puts a Drone into play when every Drone is stunned" do
    tg <- testGameAgainst "01134" ["spider-man"]
    let p = player1 tg
    inGame tg do
      before <- drones
      for_ before \d -> runNow [MinionMessage d (MinionStunned GameRef)]
      _ <- revealNow p Treacheries.swarmAttack
      pass
      after <- drones
      length after `shouldBe` length before + 1
      stunned <- getsGame $ map (Minion.minionStunned . toAttrs) . HashMap.elems . gameMinions
      or stunned `shouldBe` False
