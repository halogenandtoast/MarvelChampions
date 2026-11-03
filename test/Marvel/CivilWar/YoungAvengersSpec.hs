module Marvel.CivilWar.YoungAvengersSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.List (nub)
import Marvel.CivilWar.Cards qualified as C
import Marvel.EncounterCard (gatherEncounterSet)
import Marvel.EncounterSet qualified as E
import Marvel.Keyword qualified as K
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as M
import Test.Hspec qualified as H
import TestImport

putFree :: IdentityId -> PlayerCard -> GameAppT ()
putFree you card = L.runQueueT (L.capture $ L.putIntoPlay you card) >>= resolve

spec :: Spec
spec = describe "Synthezoid Young Avengers" do
  it "gathers all five distinct physical cards" do
    cards <- gatherEncounterSet E.CwYoungAvengers
    length cards `H.shouldBe` 5
    length (nub $ map toCardCode cards) `H.shouldBe` 5

  it "Patriot enters tough and gives the enemy leader tough" do
    tg <- testGame ["spider-man"]
    inGame tg do
      leader <- villainIdNow
      patriot <- addMinion (player1 tg) C.cw57055
      getCharacterStatusCount (MinionCharacter patriot) ToughStatus `shouldReturn` 1
      getCharacterStatusCount (VillainCharacter leader) ToughStatus `shouldReturn` 1

  it "Hawkeye's attacks have piercing and ranged" do
    tg <- testGame ["spider-man"]
    inGame tg do
      hawkeye <- addMinion (player1 tg) C.cw57056
      keywords <- getEnemyKeywords (EnemyMinionId hawkeye)
      keywords `shouldSatisfy` elem K.Piercing
      keywords `shouldSatisfy` elem K.Ranged

  it "Hawkeye lets the revealing player choose an upgrade to discard" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      first <- putInHand you "01008"
      second <- putInHand you "01008"
      putFree you first
      putFree you second
      hawkeye <- encounterCard C.cw57056
      start [RevealEncounterCard you hawkeye]
      choose "discard the second Web-Shooter" (isTargetChoice $ upgradeIdOf second)
      getsGame (HashMap.member (upgradeIdOf first) . gameUpgrades) `shouldReturn` True
      getsGame (HashMap.member (upgradeIdOf second) . gameUpgrades) `shouldReturn` False

  it "Stature has guard and discards only the revealing player's support" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let one = player1 tg
          two = player2 tg
      own <- putInHand one "01091"
      teammate <- putInHand two "01091"
      putFree one own
      putFree two teammate
      stature <- addMinion one C.cw57057
      getEnemyKeywords (EnemyMinionId stature) >>= (`shouldSatisfy` elem K.Guard)
      getsGame (HashMap.member (supportIdOf own) . gameSupports) `shouldReturn` False
      getsGame (HashMap.member (supportIdOf teammate) . gameSupports) `shouldReturn` True

  for_ [C.cw57056,C.cw57057] $ \def ->
    it (show (toCardCode def) <> " has no discard or surge when no eligible card exists") do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- stackEncounterDeck [M.hydraMercenary]
        _ <- addMinion (player1 tg) def
        dealtTo (player1 tg) `shouldReturn` []

  it "Teenage Superheroes repeats every minion's reveal ability" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      _ <- addMinion (player1 tg) C.cw57013
      _ <- addMinion (player2 tg) C.cw56074
      before <- mainSchemeThreat
      card <- encounterCard C.cw57058
      resolve [RevealEncounterCard (player1 tg) card]
      mainSchemeThreat `shouldReturn` (before + 3)

  it "Teenage Superheroes surges when no minion has a reveal ability" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- addMinion (player1 tg) M.hydraMercenary
      [next] <- stackEncounterDeck [M.hydraSoldier]
      card <- encounterCard C.cw57058
      resolve [RevealEncounterCard (player1 tg) card]
      dealtTo (player1 tg) `shouldReturn` [next]

  it "Teenage Superheroes repeats surge as a reveal ability" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      runner <- encounterCard M.weaponsRunner
      resolve [PutEncounterCardIntoPlay you runner]
      [next] <- stackEncounterDeck [M.hydraSoldier]
      card <- encounterCard C.cw57058
      resolve [RevealEncounterCard you card]
      dealtTo you `shouldReturn` [next]

  it "Young Avengers shares its first-minion surge limit across teammates" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let one = player1 tg
          two = player2 tg
      side <- encounterCard C.cw57059
      resolve [RevealEncounterCard one side]
      sideSchemeThreat (SideSchemeId $ unCardId $ ecCardId side) `shouldReturn` 5
      _ <- stackEncounterDeck $ replicate 4 M.hydraMercenary
      _ <- addMinion one C.cw57055
      length <$> dealtTo one `shouldReturn` 1
      _ <- addMinion two C.cw57057
      dealtTo two `shouldReturn` []
