module Marvel.Encounter.AgeOfApocalypseStandardIIISpec (spec) where

import Marvel.Query
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import TestImport
import Marvel.AgeOfApocalypse.StandardIII
import Marvel.Environment.Cards qualified as Environments
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Minion.Cards qualified as Minions

setupPursuit :: IdentityId -> GameAppT EnvironmentId
setupPursuit you = do
  _ <- revealNow you Environments.pursuedByThePastFront
  resolve []
  selectJust (EnvironmentIs Environments.pursuedByThePastFront)

pursue :: IdentityId -> Natural -> GameAppT ()
pursue you n = do
  messages <- L.runQueueT $ L.capture $ addPursuit you n
  resolve messages

spec :: Spec
spec = describe "Age of Apocalypse Standard III" do
  it "ignores queued pursuit after its activating hero is eliminated while another player survives" do
    tg <- testGame ["spider-man", "captain-marvel"]
    inGame tg do
      let p = player1 tg
          survivor = player2 tg
      env <- setupPursuit p
      pursue p 4
      changeForm p
      pass
      clearHand p
      hp <- hitPointsOf p
      setDamage p (hp - 1)
      vid <- villainIdNow
      start [VillainMessage vid $ VillainAttacksWithoutBoost p]
      deferred <- L.runQueueT $ L.capture $ L.enemyActivationAfterCurrent vid (addPursuit p 1)
      runNow deferred
      choose "Spider-Sense" (isAbilityOf p)
      choose "take the fatal attack" isNoDefender
      pass
      selectAny (IdentityWithId p) `shouldReturn` False
      selectAny (IdentityWithId survivor) `shouldReturn` True
      getCountersOn env `shouldReturn` 4
      selectAny (MinionIs Minions.vulture) `shouldReturn` False

  it "finishes flipping to the front if the pursued player dies during the nemesis reveal" do
    tg <- testGame ["spider-man", "captain-marvel"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Environments.pursuedByThePastBack
      resolve []
      env <- selectJust $ EnvironmentIs Environments.pursuedByThePastBack
      runNow [IdentityMessage p IdentityDefeated]
      selectAny (IdentityWithId p) `shouldReturn` False
      runNow [CardSpecific (EnvironmentRef env) "remainingNemesis" $ toJSON p]
      pass
      selectAny (EnvironmentIs Environments.pursuedByThePastFront) `shouldReturn` True
      selectAny (EnvironmentIs Environments.pursuedByThePastBack) `shouldReturn` False
      selectAny (IdentityWithId $ player2 tg) `shouldReturn` True

  it "places pursuit without revealing a nemesis below the player-scaled threshold" do
    tg <- testGame ["spider-man"]
    inGame tg do
      env <- setupPursuit (player1 tg)
      pursue (player1 tg) 3
      runGameMessages
      getCountersOn env `shouldReturn` 3
      selectAny (MinionIs Minions.vulture) `shouldReturn` False
  it "clears every pursuit counter and reveals the player's nemesis at the threshold" do
    tg <- testGame ["spider-man"]
    inGame tg do
      env <- setupPursuit (player1 tg)
      pursue (player1 tg) 4
      runGameMessages
      resolve []
      getCountersOn env `shouldReturn` 0
      selectAny (MinionIs Minions.vulture) `shouldReturn` True
  it "Dark Designs schemes only when pursuit counters remain after its placement" do
    tg <- testGame ["spider-man"]
    inGame tg do
      env <- setupPursuit (player1 tg)
      pursue (player1 tg) 3
      runGameMessages
      before <- mainSchemeThreatNow
      _ <- revealNow (player1 tg) Treacheries.darkDesigns
      resolve []
      getCountersOn env `shouldReturn` 0
      mainSchemeThreatNow `shouldReturn` before
  it "Evil Alliance places three pursuit counters when no nemesis minion can activate" do
    tg <- testGame ["spider-man"]
    inGame tg do
      env <- setupPursuit (player1 tg)
      _ <- revealNow (player1 tg) Treacheries.evilAlliance
      resolve []
      getCountersOn env `shouldReturn` 3
  it "Sinister Strike surges in alter-ego while its new pursuit counter remains" do
    tg <- testGame ["spider-man"]
    inGame tg do
      env <- setupPursuit (player1 tg)
      _ <- revealNow (player1 tg) Treacheries.sinisterStrike
      resolve []
      getCountersOn env `shouldReturn` 1
