module Marvel.Encounter.GliderSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Matchers
import Marvel.Query
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Minion.Cards qualified as Minions
import Marvel.Game qualified as Game
import Marvel.MainScheme.Types qualified as Main
import Marvel.Message.Lifted qualified as L
import Marvel.MainScheme.MainSchemes.SmVenomGoblin qualified as Goblin
import TestImport

scheme :: CardCode -> GameAppT MainSchemeId
scheme code = getsGame $ \g -> fromMaybe (error "main scheme missing") $ headMay [sid | (sid,s) <- HashMap.toList (gameMainSchemes g), toCardCode (getCardDef s) == code]

threat :: MainSchemeId -> GameAppT Natural
threat sid = getsGame $ maybe 0 Main.getMainSchemeThreat . HashMap.lookup sid . gameMainSchemes

spec :: Spec
spec = describe "Venom Goblin glider rules" do
  it "keeps all three player targets but routes unqualified encounter effects to Midtown" do
    tg <- testGameAgainst "27113" ["spider-man"]
    inGame tg do
      mid <- scheme "27118"
      Game.getGliderSchemes `shouldReturn` [mid]
      allMain <- selectList MainScheme
      liftIO $ length allMain `shouldBe` 3
      selectList EncounterMainScheme `shouldReturn` [SchemeMainSchemeId mid]
  it "places each acceleration token on the glider scheme regardless of the named scheme" do
    tg <- testGameAgainst "27113" ["spider-man"]
    inGame tg do
      lower <- scheme "27117"
      mid <- scheme "27118"
      resolve [PlaceAccelerationToken lower]
      tokens <- getsGame $ maybe 0 (Main.mainSchemeAccelerationTokens . toAttrs) . HashMap.lookup mid . gameMainSchemes
      liftIO $ tokens `shouldBe` 1
      Game.getAccelerationCount `shouldReturn` 1
  it "keeps a token on its scheme when the glider moves and accelerates only that scheme" do
    tg <- testGameAgainst "27113" ["spider-man"]
    inGame tg do
      lower <- scheme "27117"
      mid <- scheme "27118"
      upper <- scheme "27119"
      resolve [PlaceAccelerationToken mid]
      resolve =<< L.runQueueT (L.capture $ Goblin.moveGlider lower)
      resolve [AccelerateMainScheme]
      threat lower `shouldReturn` 2
      threat mid `shouldReturn` 4
      threat upper `shouldReturn` 1
  it "moves the counter without moving acceleration tokens" do
    tg <- testGameAgainst "27113" ["spider-man"]
    inGame tg do
      lower <- scheme "27117"
      mid <- scheme "27118"
      resolve [PlaceAccelerationToken mid]
      resolve =<< L.runQueueT (L.capture $ Goblin.moveGlider lower)
      Game.getGliderSchemes `shouldReturn` [lower]
      tokens <- getsGame $ maybe 0 (Main.mainSchemeAccelerationTokens . toAttrs) . HashMap.lookup mid . gameMainSchemes
      liftIO $ tokens `shouldBe` 1

  it "a crisis icon blocks only the main scheme with the glider" do
    tg <- testGameAgainst "27113" ["spider-man"]
    inGame tg do
      lower <- scheme "27117"
      mid <- scheme "27118"
      _ <- revealNow (player1 tg) SideSchemes.underAttack
      pass
      candidates <- selectList ThwartableScheme
      liftIO $ candidates `shouldContain` [SchemeMainSchemeId lower]
      liftIO $ candidates `shouldNotContain` [SchemeMainSchemeId mid]
  it "patrol blocks only the main scheme with the glider" do
    tg <- testGameAgainst "27113" ["spider-man"]
    inGame tg do
      lower <- scheme "27117"
      mid <- scheme "27118"
      _ <- revealNow (player1 tg) Minions.smSymbioticThrall
      _ <- revealNow (player1 tg) SideSchemes.smFesteringMass
      candidates <- selectList SchemeYouCanThwart
      liftIO $ candidates `shouldContain` [SchemeMainSchemeId lower]
      liftIO $ candidates `shouldNotContain` [SchemeMainSchemeId mid]
