module Marvel.CivilWar.MightyAvengersSpec (spec) where

import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (withLeader)
import Marvel.Villain.Types qualified as V
import Marvel.Matchers
import Marvel.Query
import TestImport

spec :: Spec
spec = describe "Civil War Mighty Avengers minions" do
  it "Yellow Jacket enters tough and gives the enemy leader tough" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- withLeader "57040"
      mid <- addMinion (player1 tg) C.cw56073
      getsGame (maybe False (minionTough . toAttrs) . lookup mid . gameMinions) `shouldReturn` True
      getsGame (maybe False (V.villainTough . toAttrs) . lookup vid . gameVillains) `shouldReturn` True

  it "Wasp places one threat on the main scheme when revealed" do
    tg <- testGame ["spider-man"]
    inGame tg do
      before <- mainSchemeThreat
      _ <- addMinion (player1 tg) C.cw56074
      mainSchemeThreat `shouldReturn` (before + 1)

  it "U.S. Agent damages the revealing player and enters tough" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      mid <- addMinion you C.cw56075
      damageOf you `shouldReturn` 1
      getsGame (maybe False (minionTough . toAttrs) . lookup mid . gameMinions) `shouldReturn` True

  it "Yellow Jacket's boost gives the enemy leader tough without putting the minion into play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- withLeader "57040"
      _ <- stackEncounterDeck [C.cw56073]
      resolve [VillainMessage vid VillainSchemes]
      getsGame (maybe False (V.villainTough . toAttrs) . lookup vid . gameVillains) `shouldReturn` True
      selectList (MinionIs C.cw56073) `shouldReturn` []

  it "Wasp's boost places additional threat during a scheme activation" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- withLeader "57040"
      before <- mainSchemeThreat
      _ <- stackEncounterDeck [C.cw56074]
      resolve [VillainMessage vid VillainSchemes]
      mainSchemeThreat `shouldReturn` (before + 2)

  it "U.S. Agent's boost damages the player during a scheme activation" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- withLeader "57040"
      _ <- stackEncounterDeck [C.cw56075]
      resolve [VillainMessage vid VillainSchemes]
      damageOf (player1 tg) `shouldReturn` 1
