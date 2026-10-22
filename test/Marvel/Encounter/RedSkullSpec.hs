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
      liftIO $ sort (map (unCardCode . toCardCode . getCardDef) deck)
        `shouldBe` sort (map (unCardCode . toCardCode) [Cards.theSleeperAwakened, Cards.prisonCamps, Cards.censorThePast, Cards.hydraReinforcements, Cards.massChaos])
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

  describe "The Sleeper Awakened" do
    it "cannot leave play while The Sleeper is in play, by defeat, discard or removal, and leaves the game with him" do
      tg <- redSkull
      let p = player1 tg
      inGame tg do
        awakened <- revealNow p Cards.theSleeperAwakened
        pass
        let sid = sideSchemeOf awakened
            inPlay = getsGame (HashMap.member sid . gameSideSchemes)
        [sleeper] <- getsGame (HashMap.keys . HashMap.filter ((== Minions.theSleeper) . getCardDef) . gameMinions)
        -- thwarted to nothing, discarded, removed: it stays
        resolve [SideSchemeMessage sid (SideSchemeThwarted (IdentityRef p) 1)]
        inPlay `shouldReturn` True
        resolve [RemoveFromPlay (SideSchemeRef sid)]
        inPlay `shouldReturn` True
        resolve [RemoveFromGame (SideSchemeRef sid)]
        inPlay `shouldReturn` True
        -- The Sleeper defeated, it is removed from the game: not discarded
        -- toughness takes the first
        resolve [MinionMessage sleeper (MinionDamaged GameRef (toDamage 99 FromAbility))]
        inPlay `shouldReturn` True
        resolve [MinionMessage sleeper (MinionDamaged GameRef (toDamage 99 FromAbility))]
        minionDamageOf sleeper `shouldReturn` Nothing
        inPlay `shouldReturn` False
        Pile _ discard <- getPile SideSchemeDeckKey
        map getCardDef discard `shouldBe` []
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "04140")
