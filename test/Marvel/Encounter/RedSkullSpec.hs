module Marvel.Encounter.RedSkullSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.List (sort)
import Marvel.Area (DeckKey (SideSchemeDeckKey), Pile (..))
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as Cards
import TestImport

redSkull :: IO TestGame
redSkull = testGameAgainst "04125" ["spider-man"]

sideSchemesWith :: CardDef -> GameAppT [SideSchemeId]
sideSchemesWith def = getsGame (HashMap.keys . HashMap.filter ((== def) . getCardDef) . gameSideSchemes)

redHouse :: GameAppT SideSchemeId
redHouse = sideSchemesWith Cards.theRedHouse >>= \case
  [sid] -> pure sid
  _ -> error "The Red House is not in play"

spec :: Spec
spec = describe "Red Skull" do
  it "sets up with The Red House in play, the other side schemes in their own deck, and The Sleeper aside" do
    tg <- redSkull
    inGame tg do
      _ <- redHouse
      Pile deck discard <- getPile SideSchemeDeckKey
      -- with the recommended Hydra Patrol set's side scheme too
      liftIO $ sort (map (unCardCode . toCardCode . getCardDef) deck)
        `shouldBe` sort (map (unCardCode . toCardCode) [Cards.theSleeperAwakened, Cards.prisonCamps, Cards.censorThePast, Cards.hydraReinforcements, Cards.massChaos, Cards.hydraPatrol])
      liftIO $ discard `shouldBe` []
      encounter <- scenarioEncounterDeckNow
      liftIO $ filter ((== SideSchemeType) . cdCardType . getCardDef) encounter `shouldBe` []
      liftIO $ filter ((== Minions.theSleeper) . getCardDef) encounter `shouldBe` []

  it "Red Skull cannot take damage while The Red House is in play" do
    tg <- redSkull
    inGame tg do
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainDamaged GameRef (toDamage 3 FromAbility))]
      villainDamageNow `shouldReturn` 0
      sid <- redHouse
      resolve [RemoveFromPlay (SideSchemeRef sid)]
      resolve [VillainMessage vid (VillainDamaged GameRef (toDamage 3 FromAbility))]
      villainDamageNow `shouldReturn` 3

  it "a hero thwarting The Red House may use their ATK instead of their THW" do
    tg <- redSkull
    let you = player1 tg
    inGame tg do
      sid <- redHouse
      sideSchemeThreat sid `shouldReturn` 3
      changeForm you
      choose "basic thwart" (isHeroThwart you)
      choose "thwart The Red House" (isTargetChoice sid)
      -- Spider-Man: THW 1, ATK 2
      choose "use ATK" (isLabel "$label.thwartWithAtk")
      sideSchemeThreat sid `shouldReturn` 1

  it "step one of the villain phase reveals the side-scheme deck's top card; a defeated one goes to its discard pile, shuffled back without an acceleration token" do
    tg <- redSkull
    inGame tg do
      card <- encounterCard Cards.hydraReinforcements
      resolve [CreateDeck SideSchemeDeckKey [card]]
      tokens <- accelerationTokensNow
      resolve [AccelerateMainScheme]
      [sid] <- sideSchemesWith Cards.hydraReinforcements
      Pile deck _ <- getPile SideSchemeDeckKey
      liftIO $ deck `shouldBe` []
      resolve [RemoveFromPlay (SideSchemeRef sid)]
      -- discarded to its own pile, then made into the deck again for free
      getPile SideSchemeDeckKey `shouldReturn` Pile [card] []
      accelerationTokensNow `shouldReturn` tokens
      encounterDiscardCodes >>= liftIO . (`shouldBe` False) . elem (toCardCode Cards.hydraReinforcements)

  it "an ally thwarting The Red House may use its ATK instead of its THW" do
    tg <- redSkull
    let you = player1 tg
    inGame tg do
      sid <- redHouse
      -- Tigra: THW 1, ATK 2
      _ <- putIntoPlay you "01051"
      tigra <- allyIn "01051"
      choose "ally thwart" (isAllyThwart tigra)
      choose "thwart The Red House" (isTargetChoice sid)
      choose "use ATK" (isLabel "$label.thwartWithAtk")
      sideSchemeThreat sid `shouldReturn` 1
