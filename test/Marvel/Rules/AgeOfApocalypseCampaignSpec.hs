module Marvel.Rules.AgeOfApocalypseCampaignSpec (spec) where

import Test.Hspec qualified as Hspec
import TestImport
import Marvel.Campaign

spec :: Spec
spec = describe "Age of Apocalypse campaign log" do
  it "plays all five scenarios in the printed order" do
    let c = newCampaignOf AgeOfApocalypseCampaign StandardCampaign ["45001a", "45030a"]
    c.scenarios `Hspec.shouldBe` ["45059", "45081a", "45101b", "45118", "45184a"]
    currentScenario c `Hspec.shouldBe` Just "45059"
    (c.campaignLog.players <&> (.identity)) `Hspec.shouldBe` ["45001a", "45030a"]
  it "keeps the chosen mission on a failed scenario retry without keeping rewards" do
    let initial = newCampaignOf AgeOfApocalypseCampaign StandardCampaign ["45001a"]
        attempted = recordInLog (RecordApocalypseReward "45001a" "45176") $
          recordInLog (RecordApocalypseMission 1 "45166a") initial
        retry = retainNextEvolutionRetryChoice initial attempted
    retry.campaignLog.apocalypse.missions `Hspec.shouldBe` [(1, "45166a")]
    apocalypseRewardsFor "45001a" retry `Hspec.shouldBe` []
  it "records each success, overseer and reward once and persists them" do
    let initial = newCampaignOf AgeOfApocalypseCampaign ExpertCampaign ["45001a"]
        records = [RecordApocalypseMission 1 "45166a", RecordApocalypseMissionSuccess "45166a",
          RecordApocalypseOverseer "45179a", RecordApocalypseReward "45001a" "45176"]
        c = foldl' (flip recordInLog) initial (records <> records)
    c.campaignLog.apocalypse.successfulMissions `Hspec.shouldBe` ["45166a"]
    c.campaignLog.apocalypse.defeatedOverseers `Hspec.shouldBe` ["45179a"]
    campaignCardsOf "45001a" c `Hspec.shouldBe` ["45176"]
    eitherDecode (encode c) `Hspec.shouldBe` Right c
  it "removes Refugees from every reward column when survivors are evacuated" do
    let c = foldl' (flip recordInLog)
          (newCampaignOf AgeOfApocalypseCampaign StandardCampaign ["45001a", "45030a"])
          [RecordApocalypseReward "45001a" "45178", RecordApocalypseReward "45030a" "45178",
           RecordApocalypseReward "45030a" "45172", RemoveApocalypseReward "45178"]
    apocalypseRewardsFor "45001a" c `Hspec.shouldBe` []
    apocalypseRewardsFor "45030a" c `Hspec.shouldBe` ["45172"]
  it "does not add a duplicate reward when the submitted deck uses another printing" do
    let c = recordInLog (RecordApocalypseReward "45001a" "45013")
          (newCampaignOf AgeOfApocalypseCampaign StandardCampaign ["45001a"])
        original = starter "bishop"
        deck = original {cards = [(if code == "45013" then "04016" else code,n) | (code,n) <- original.cards]}
        adjusted = campaignDeck c deck
    adjusted.cards `Hspec.shouldSatisfy` notElem ("45013",1)
    campaignDeckShortBy c deck `Hspec.shouldBe` 1

  it "ends the campaign when the Professor was not protected" do
    let c = recordInLog RecordCampaignLost (newCampaignOf AgeOfApocalypseCampaign StandardCampaign ["45001a"])
    currentScenario c `Hspec.shouldBe` Nothing
