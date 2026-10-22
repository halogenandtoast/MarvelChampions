module Marvel.Encounter.BlackWidowNemesisSpec (spec) where

import Control.Monad.Random (evalRand)
import Data.HashMap.Strict qualified as HashMap
import Data.List (sort)
import Marvel.EncounterCard (gatherEncounterSet)
import Marvel.EncounterSet (EncounterSet (BlackWidowNemesis))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as M
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import System.Random (mkStdGen)
import TestImport

blackWidow :: IO TestGame
blackWidow = testGame ["black-widow"]

spec :: Spec
spec = describe "Black Widow's nemesis set" do
  it "is Taskmaster, Killer for Hire, 2 Hydra Mercenaries and Deadly Shot" do
    let cards = evalRand (gatherEncounterSet BlackWidowNemesis) (mkStdGen 0)
    sort (map (unCardCode . toCardCode) cards) `shouldBe` ["08026", "08027", "08028", "08028", "08029"] :: IO ()

  it "Taskmaster gets +1 ATK and +1 SCH for each upgrade the player he is engaged with controls" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      clearHand p
      _ <- putIntoPlay p "08017"
      _ <- putIntoPlay p "08024"
      taskmaster <- addMinion p Minions.taskmaster
      threat <- mainSchemeThreatNow
      resolve [MinionMessage taskmaster MinionSchemes]
      mainSchemeThreatNow `shouldReturn` (threat + 2)
      changeForm p
      resolve [MinionMessage taskmaster (MinionAttacks p)]
      damageOf p `shouldReturn` 2

  it "Taskmaster as a boost gives the villain +1 for each upgrade you control" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      clearHand p
      _ <- putIntoPlay p "08017"
      _ <- putIntoPlay p "08024"
      _ <- putIntoPlay p "08007"
      _ <- stackEncounterDeck [Minions.taskmaster]
      threat <- mainSchemeThreatNow
      vid <- villainIdNow
      resolve [VillainMessage vid VillainSchemes]
      -- Rhino's 1 SCH and 3 upgrades
      mainSchemeThreatNow `shouldReturn` (threat + 4)

  it "Taskmaster as a boost counts the upgrades of the player attacked, not the active player's" do
    tg <- testGame ["black-widow", "spider-man"]
    inGame tg do
      [p1, p2] <- players
      clearHand p1
      _ <- putIntoPlay p1 "08017"
      _ <- putIntoPlay p1 "08024"
      resolve [IdentityMessage p2 (ChangedToForm A)]
      _ <- stackEncounterDeck [Minions.taskmaster]
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainAttacks p2)]
      -- Rhino's 2 ATK: Spider-Man controls no upgrade
      damageOf p2 `shouldReturn` 2

  it "Killer for Hire enters with 3 threat and 1 more per hero, and accelerates" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      acceleration <- getAccelerationCount
      scheme <- revealNow p SideSchemes.killerForHire
      pass
      sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 4
      getAccelerationCount `shouldReturn` (acceleration + 1)

  it "Hydra Mercenary in the set is the core set's, with guard" do
    sameCard Minions.hydraMercenary Minions.hydraMercenaryBlackWidow `shouldBe` True :: IO ()
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      changeForm p
      _ <- addMinion p Minions.hydraMercenaryBlackWidow
      getsGame (map (toCardCode . M.minionCardDef . toAttrs) . HashMap.elems . gameMinions) `shouldReturn` ["08028"]
      choose "attack" (isHeroAttack p)
      vid <- villainIdNow
      offers (isTargetChoice vid) `shouldReturn` False

  it "Deadly Shot as an alter-ego: discard an upgrade and place 1 threat on the main scheme" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      _ <- putIntoPlay p "08017"
      threat <- mainSchemeThreatNow
      _ <- revealNow p Treacheries.deadlyShot
      chooseIf isTargetUpgrade
      pass
      upgradeCodes `shouldReturn` []
      mainSchemeThreatNow `shouldReturn` (threat + 1)
      damageOf p `shouldReturn` 0

  it "Deadly Shot as a hero: discard an upgrade and take 1 damage" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      changeForm p
      threat <- mainSchemeThreatNow
      _ <- revealNow p Treacheries.deadlyShot
      pass
      damageOf p `shouldReturn` 1
      mainSchemeThreatNow `shouldReturn` threat
 where
  isTargetUpgrade = \case
    TargetLabel (UpgradeRef _) _ -> True
    _ -> False
