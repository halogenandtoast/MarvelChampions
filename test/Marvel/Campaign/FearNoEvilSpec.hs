module Marvel.Campaign.FearNoEvilSpec (spec) where

import Data.List qualified as List
import Marvel.Campaign
import Marvel.Matchers
import Marvel.Query
import Test.Hspec qualified as H
import TestImport

freshCampaign :: CampaignMode -> Campaign
freshCampaign mode = newCampaignOf FearNoEvilCampaign mode ["01001a"]

withLog :: FearNoEvilLog -> Campaign -> Campaign
withLog log c = c {campaignLog=c.campaignLog {fearNoEvil=log}}

spec :: Spec
spec = describe "Fear No Evil campaign" do
  it "starts with all five interchangeable scenarios unresolved and no predetermined history" do
    let c = freshCampaign StandardCampaign
    fearNoEvilUnresolved c `H.shouldBe` fearNoEvilScenarioPool
    c.scenarios `H.shouldBe` []
    campaignWon c `H.shouldBe` False

  it "fails a scenario on its third progress mark and caps further progress" do
    let code = "60128a"
        log = progressFearNoEvil [code,code,code,code] noFearNoEvilLog
        c = withLog log $ freshCampaign StandardCampaign
    List.lookup code log.progress `H.shouldBe` Just 3
    List.lookup code log.resolved `H.shouldBe` Just False
    fearNoEvilUnresolved c `shouldNotContain` [code]

  it "records the chosen order and retains the assigned underling after an expert loss" do
    let selected = recordInLog (RecordFearNoEvilScenario "60121a") $ freshCampaign ExpertCampaign
        assigned = recordInLog (RecordFearNoEvilUnderling "60121a" "60086") selected
        lost = recordInLog (RecordFearNoEvilOutcome False) assigned
        saved = retainNextEvolutionRetryChoice selected lost
    saved.scenarios `H.shouldBe` ["60121a"]
    saved.campaignLog.fearNoEvil.underlings `H.shouldBe` [("60121a","60086")]
    List.lookup "60121a" saved.campaignLog.fearNoEvil.progress `H.shouldBe` Just 1
    saved.campaignLog.fearNoEvil.selectedScenario `H.shouldBe` Nothing
    saved.campaignLog.fearNoEvil.gamesPlayed `H.shouldBe` 1

  it "does not add an expert defeat mark in standard campaign mode" do
    let c = recordInLog (RecordFearNoEvilOutcome False)
          $ recordInLog (RecordFearNoEvilScenario "60142a") $ freshCampaign StandardCampaign
    c.campaignLog.fearNoEvil.progress `H.shouldBe` []
    c.campaignLog.fearNoEvil.resolved `H.shouldBe` []

  it "unlocks Kingpin after all five scenarios have been completed or failed" do
    let log = noFearNoEvilLog {resolved=zip fearNoEvilScenarioPool [True,False,True,False,True]}
        c = withLog log $ freshCampaign StandardCampaign
    currentScenario c `H.shouldBe` Just "60161a"
    campaignWon c `H.shouldBe` False
    let won = recordInLog (RecordFearNoEvilOutcome True) $ recordInLog (RecordFearNoEvilScenario "60161a") c
    campaignWon won `H.shouldBe` True
    currentScenario won `H.shouldBe` Nothing

  it "reaches the first player turn with a chosen scenario and a persistent underling assignment" do
    let c = freshCampaign StandardCampaign
        setup = fromMaybe (error "Fear No Evil setup missing") $ recommendedSetup "60134a"
    (app,ran,logged) <- dealtGameWith (campaignL ?~ c) setup [starter "spider-man"] 1
    seats <- runGameApp app (getsGame gameSeats)
    let tg = TestGame app ran logged seats
    inGame tg do
      toFirstTurn
      campaign <- getsGame $ fromMaybe (error "campaign missing") . gameCampaign
      length campaign.scenarios `shouldBe` 1
      length campaign.campaignLog.fearNoEvil.underlings `shouldBe` 1
      sum (map snd campaign.campaignLog.fearNoEvil.progress) `shouldBe` 2
      selectListCount AnyVillain `shouldReturn` 1
      resolve [GameOver Won]
      won <- getsGame $ fromMaybe (error "campaign missing") . gameCampaign
      length won.campaignLog.fearNoEvil.resolved `shouldBe` 1
      won.campaignLog.fearNoEvil.gamesPlayed `shouldBe` 1
