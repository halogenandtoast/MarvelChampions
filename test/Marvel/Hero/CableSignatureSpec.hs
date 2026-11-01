module Marvel.Hero.CableSignatureSpec (spec) where

import Data.Maybe (fromJust)
import Marvel.GameValue
import Marvel.Trait (Trait (..))
import Marvel.MainScheme.Types (mainSchemeAccelerationTokens)
import Marvel.Window qualified as W
import Marvel.Scenario.Types (scenarioEncounterDeck)
import Data.HashMap.Strict qualified as HashMap
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.SideScheme.Types qualified as SideScheme
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity (currentIdentity, PlayerIdentitySide (..))
import Marvel.Game (getPlayerVictoryDisplay)
import Marvel.Modifier
import Marvel.Query
import Marvel.Matchers
import TestImport


asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

cardAbility :: PlayerCard -> Natural -> Choice -> Bool
cardAbility card n choice = isAbilityOfCard card choice && case choice of
  UseAbility ab -> abilityIndex ab == n
  _ -> False

purge :: GameAppT (Maybe SideSchemeId)
purge = getsGame $ headMay . HashMap.keys . HashMap.filter ((== "40006") . toCardCode . getCardDef) . gameSideSchemes

spec :: Spec
spec = describe "Cable signature events" do
  it "Bodyslide changes form without using the ordinary once-per-round form change" do
    tg <- testGame ["cable"]
    inGame tg do
      let p = player1 tg
      clearHand p
      card <- putInHand p "40002"
      playWith card []
      pass
      (\case HeroSide _ -> True; _ -> False) . currentIdentity <$> getPlayer p `shouldReturn` True
      changeForm p
      pass
      (\case AlterEgoSide _ -> True; _ -> False) . currentIdentity <$> getPlayer p `shouldReturn` True

  it "Mind Scan removes its base three threat with no completed side schemes" do
    tg <- testGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      sid <- mainSchemeIdNow
      resolve [MainSchemeMessage sid (MainSchemePlaceThreat 5)]
      card <- putInHand p "40003"
      pay <- putInHand p "01089"
      playWith card [pay]
      choose "main scheme" (isTargetChoice sid)
      pass
      mainSchemeThreatNow `shouldReturn` 2

  it "Mind Scan counts player side schemes in the victory display" do
    tg <- testGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      purge >>= traverse_ (\sid -> resolve [AddToVictoryDisplay (SideSchemeRef sid)])
      sid <- mainSchemeIdNow
      resolve [MainSchemeMessage sid (MainSchemePlaceThreat 6)]
      card <- putInHand p "40003"
      pay <- putInHand p "01089"
      playWith card [pay]
      choose "main scheme" (isTargetChoice sid)
      pass
      mainSchemeThreatNow `shouldReturn` 2

  it "Telekinetic Blast deals six base damage" do
    tg <- testGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putInHand p "40005"
      pay <- putInHand p "01088"
      extra <- putInHand p "01091"
      playWith card [pay, extra]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 6

  it "Telekinetic Blast increases by each defeated player side scheme" do
    tg <- testGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      purge >>= traverse_ (\sid -> resolve [AddToVictoryDisplay (SideSchemeRef sid)])
      card <- putInHand p "40005"
      pay <- putInHand p "01088"
      extra <- putInHand p "01091"
      playWith card [pay, extra]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 7

  it "Precognition with no victory schemes does not discard any encounter cards" do
    tg <- testGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putInHand p "40004"
      before <- length . scenarioEncounterDeck . toAttrs . gameScenario <$> getGame
      playWith card []
      pass
      length . scenarioEncounterDeck . toAttrs . gameScenario <$> getGame `shouldReturn` before
