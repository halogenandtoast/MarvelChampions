module Marvel.Encounter.VenomGoblinSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Cards qualified as Minions
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Modifier
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Query
import Marvel.Game qualified as Game
import Marvel.MainScheme.MainSchemes.SmVenomGoblin qualified as Goblin
import Marvel.Message.Lifted qualified as L
import Marvel.SideScheme.Cards qualified as SideSchemes
import TestImport

settle :: GameAppT ()
settle = go (150 :: Int)
 where
  go 0 = error "Venom Goblin did not settle"
  go n = openQuestion >>= \case
    Just (_, QuestionLabel "$label.yourTurn" _ _) -> pure ()
    Just (you,q) -> answerQuestion you (fromMaybe 0 $ headMay [i | (i,c) <- zip [0..] (choicesOf q), isNoResponse c || isNoDefender c]) >> go (n-1)
    Nothing -> pure ()

scheme :: CardCode -> GameAppT MainSchemeId
scheme code = getsGame $ \g -> fromMaybe (error "main scheme missing") $ headMay [sid | (sid,s) <- HashMap.toList (gameMainSchemes g), toCardCode (getCardDef s) == code]

spec :: Spec
spec = describe "Venom Goblin scenario" do
  it "sets up Lower, Midtown, and Upper Manhattan at their independent starting threat" do
    tg <- testGameAgainst "27113" ["spider-man"]
    inGame tg do
      mainSchemeThreatNow `shouldReturn` 3
      Game.getGliderSchemes >>= (\xs -> liftIO $ length xs `shouldBe` 1)
      Goblin.symbioteEnvironmentCount `shouldReturn` 0
  it "Lower Manhattan's Special places one threat on every scheme" do
    tg <- testGameAgainst "27113" ["spider-man"]
    inGame tg do
      lower <- scheme "27117"
      runNow =<< L.runQueueT (L.capture $ Goblin.resolveGoblinSpecial (player1 tg) lower)
      settle
      mainSchemeThreatNow `shouldReturn` 6
  it "Festering Mass supplies the symbiote environment condition" do
    tg <- testGameAgainst "27113" ["spider-man"]
    inGame tg do
      _ <- revealNow (player1 tg) SideSchemes.smFesteringMass
      settle
      Goblin.symbioteEnvironmentCount `shouldReturn` 1
      lower <- scheme "27117"
      before <- mainSchemeThreatNow
      runNow =<< L.runQueueT (L.capture $ Goblin.resolveGoblinSpecial (player1 tg) lower)
      settle
      mainSchemeThreatNow `shouldReturn` (before + 4)
  it "Upper Manhattan's Special discards one chosen hand card" do
    tg <- testGameAgainst "27113" ["spider-man"]
    inGame tg do
      upper <- scheme "27119"
      before <- length <$> handOf (player1 tg)
      runNow =<< L.runQueueT (L.capture $ Goblin.resolveGoblinSpecial (player1 tg) upper)
      settle
      (length <$> handOf (player1 tg)) `shouldReturn` (before - 1)
  it "flips a completed Manhattan scheme and moves its acceleration tokens to the least-threat scheme" do
    tg <- testGameAgainst "27113" ["spider-man"]
    inGame tg do
      mid <- scheme "27118"
      upper <- scheme "27119"
      resolve [PlaceAccelerationToken mid]
      runNow [MainSchemeMessage mid $ MainSchemePlaceThreat 10]
      settle
      present <- getsGame $ HashMap.member mid . gameMainSchemes
      liftIO $ present `shouldBe` False
      Goblin.symbioteEnvironmentCount `shouldReturn` 1
      Game.getGliderSchemes `shouldReturn` [upper]
      Game.getAccelerationCount `shouldReturn` 1
  it "loses when a second Manhattan scheme flips, while Festering Mass does not cause an early loss" do
    tg <- testGameAgainst "27113" ["spider-man"]
    inGame tg do
      _ <- revealNow (player1 tg) SideSchemes.smFesteringMass
      settle
      mid <- scheme "27118"
      lower <- scheme "27117"
      runNow [MainSchemeMessage mid $ MainSchemePlaceThreat 10]
      settle
      Goblin.symbioteEnvironmentCount `shouldReturn` 1
      getsGame gameState `shouldReturn` InProgress
      runNow [MainSchemeMessage lower $ MainSchemePlaceThreat 10]
      settle
      getsGame gameState `shouldReturn` Finished Lost
  it "Upper Manhattan additionally discards four deck cards while infested" do
    tg <- testGameAgainst "27113" ["spider-man"]
    inGame tg do
      _ <- revealNow (player1 tg) SideSchemes.smFesteringMass
      settle
      upper <- scheme "27119"
      before <- length . unDeck . (.playerIdentityDeck) <$> playerAttrs (player1 tg)
      runNow =<< L.runQueueT (L.capture $ Goblin.resolveGoblinSpecial (player1 tg) upper)
      settle
      after <- length . unDeck . (.playerIdentityDeck) <$> playerAttrs (player1 tg)
      liftIO $ after `shouldBe` before - 4

  it "infestation grants Berserker quickstrike, Thrall patrol, and Monstrosity three hit points" do
    tg <- testGameAgainst "27113" ["spider-man"]
    inGame tg do
      _ <- revealNow (player1 tg) Minions.smSymbioticBerserker
      settle
      _ <- revealNow (player1 tg) Minions.smSymbioticThrall
      settle
      _ <- revealNow (player1 tg) Minions.smSymbioticMonstrosity
      settle
      berserker <- selectJust (MinionIs Minions.smSymbioticBerserker)
      thrall <- selectJust (MinionIs Minions.smSymbioticThrall)
      monstrosity <- selectJust (MinionIs Minions.smSymbioticMonstrosity)
      before <- getModifiers berserker
      liftIO $ before `shouldNotContain` [KeywordModifier Quickstrike]
      _ <- revealNow (player1 tg) SideSchemes.smFesteringMass
      settle
      berserkerMods <- getModifiers berserker
      thrallMods <- getModifiers thrall
      monsterMods <- getModifiers monstrosity
      liftIO $ berserkerMods `shouldContain` [KeywordModifier Quickstrike]
      liftIO $ thrallMods `shouldContain` [KeywordModifier Patrol]
      liftIO $ monsterMods `shouldContain` [HitPointModifier 3]
  it "We Are One adds three ATK and three SCH to the attached villain" do
    tg <- testGameAgainst "27113" ["spider-man"]
    inGame tg do
      _ <- revealNow (player1 tg) Attachments.smWeAreOne
      settle
      vid <- villainIdNow
      modifiers <- getModifiers vid
      liftIO $ modifiers `shouldContain` [AttackModifier 3, SchemeModifier 3]
