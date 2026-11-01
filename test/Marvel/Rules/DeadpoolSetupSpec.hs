module Marvel.Rules.DeadpoolSetupSpec (spec) where

import Data.List (nub)
import Marvel.Aspect (Aspect (..))
import Marvel.EncounterSet (EncounterSet (DreadpoolSet))
import Marvel.Scenario.Types (Attrs (scenarioSetAsideCards))
import TestImport

setupTable :: [DeckList] -> IO TestGame
setupTable decks = do
  let setup = fromMaybe (error "no Rhino setup") $ recommendedSetup "01094"
  (app, ran, logged) <- dealtGameLogged setup decks 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

-- Replace Justice by legal Pool cards while preserving the deck's size. Adam's
-- singleton rule also preserves the number of cards of each chosen aspect.
withPool :: Bool -> DeckList -> DeckList
withPool singleton deck = deck {aspects = if singleton then [] else ["pool"], cards = kept <> map (, 1) replacement}
 where
  justice code = cdAspect (lookupPlayerCard code) == Just Justice
  slots = sum [n | (code, n) <- deck.cards, justice code]
  kept = [(code, n) | (code, n) <- deck.cards, not (justice code)]
  available = [code | (code, n) <- (starter "deadpool").cards, cdAspect (lookupPlayerCard code) == Just Pool, _ <- [1 .. n]]
  replacement = take slots $ if singleton then nub available else available

spec :: Spec
spec = describe "Deadpool Pool setup" do
  it "adds one Crisis and sets the rest of Dreadpool aside even with two Pool players" do
    let other = withPool False (starter "spider-man")
    (deckBuildingError other `shouldBe` Nothing) :: IO ()
    tg <- setupTable [starter "deadpool", other]
    inGame tg do
      deck <- scenarioEncounterDeckNow
      length (filter ((== "44037") . toCardCode) deck) `shouldBe` 1
      aside <- getsGame $ scenarioSetAsideCards . toAttrs . gameScenario
      map toCardCode (filter ((== Just DreadpoolSet) . cdEncounterSet . getCardDef) aside)
        `shouldMatchList` ["44038", "44039", "44040", "44041", "44041", "44042"]

  it "does not add Crisis when Cable splashes a Pool player side scheme" do
    let deck = (starter "cable") {cards = ("44024", 1) : (starter "cable").cards}
    (deckBuildingError deck `shouldBe` Nothing) :: IO ()
    tg <- setupTable [deck]
    inGame tg do
      codes <- map toCardCode <$> scenarioEncounterDeckNow
      ("44037" `elem` codes) `shouldBe` False
      aside <- getsGame $ scenarioSetAsideCards . toAttrs . gameScenario
      filter ((== Just DreadpoolSet) . cdEncounterSet . getCardDef) aside `shouldBe` []

  it "infers Adam Warlock's Pool choice when imported aspect metadata is absent" do
    let deck = withPool True (starter "adam-warlock")
    (deckBuildingError deck `shouldBe` Nothing) :: IO ()
    tg <- setupTable [deck]
    inGame tg do
      attrs <- playerAttrs (player1 tg)
      (Pool `elem` attrs.playerIdentityChosenAspects) `shouldBe` True
      length attrs.playerIdentityChosenAspects `shouldBe` 4
      codes <- map toCardCode <$> scenarioEncounterDeckNow
      ("44037" `elem` codes) `shouldBe` True
