module Marvel.Campaign.ShadowlandSpec (spec) where

import Marvel.Campaign
import Marvel.Campaign.Shadowland (runShadowlandStep)
import Marvel.Message.Lifted qualified as L
import Marvel.Matchers
import Marvel.Query
import TestImport

campaignGame :: CampaignMode -> Int -> IO TestGame
campaignGame = campaignGameFor ["spider-man"]

campaignGameFor :: [Text] -> CampaignMode -> Int -> IO TestGame
campaignGameFor heroes mode completed = do
  let decks = map starter heroes
      c = (newCampaignOf ShadowlandCampaign mode (map (.hero) decks)) {completed}
      code = fromMaybe (error "campaign finished") (currentScenario c)
      setup = fromMaybe (error "scenario not registered") (recommendedSetup code)
  (app, ran, logged) <- dealtGameWith (campaignL ?~ c) setup decks 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

spec :: Spec
spec = describe "Shadowland campaign" do
  it "lets a defeated expert player rejoin at full health for one encounter card" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
          c = recordInLog (RecordHitPoints "01001a" 0)
            $ (newCampaignOf ShadowlandCampaign ExpertCampaign ["01001a"]) {completed=1}
      resolve =<< L.runQueueT (L.capture $ runShadowlandStep c $ CampaignSetupThen "shadowland:prepared")
      pass
      damageOf you `shouldReturn` 0
      length . playerIdentityEncounterCards . toAttrs <$> getPlayer you `shouldReturn` 1

  for_ [(0,3),(1,28),(2,2)] $ \(completed,threat) ->
    it ("sets up campaign scenario " <> show (completed+1) <> " with corruption") do
      tg <- campaignGame StandardCampaign completed
      inGame tg do
        mainSchemeThreatNow `shouldReturn` threat
        selectListCount (UpgradeNamed "Creeping Corruption") `shouldReturn` 1

  it "records a defeated protege and advances after a victory" do
    tg <- campaignGame StandardCampaign 0
    inGame tg do
      resolve [GameOver Won]
      campaign <- getsGame (fromMaybe (error "missing campaign") . gameCampaign)
      campaign.completed `shouldBe` 1
      length campaign.campaignLog.shadowland.removedProteges `shouldBe` 1
      currentScenario campaign `shouldBe` Just "65038a"

  it "permits replaying an expert finale after defeat" do
    tg <- campaignGame ExpertCampaign 2
    inGame tg do
      resolve [GameOver Lost]
      campaign <- getsGame (fromMaybe (error "missing campaign") . gameCampaign)
      campaign.lost `shouldBe` False
      currentScenario campaign `shouldBe` Just "65060a"

  it "records Daredevil's matching villain as removed when Daredevil is a campaign hero" do
    tg <- campaignGameFor ["daredevil"] StandardCampaign 0
    inGame tg do
      campaign <- getsGame (fromMaybe (error "missing campaign") . gameCampaign)
      campaign.campaignLog.shadowland.removedProteges `shouldBe` ["65013a"]
      selectAny (VillainWithCardCode "65013a") `shouldReturn` False
