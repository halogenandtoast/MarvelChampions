module Marvel.Campaign.NextEvolutionSpec (spec) where

import Marvel.Query
import Marvel.Matchers
import Marvel.Campaign
import Marvel.Environment.Cards qualified as Environments
import TestImport
import Test.Hspec qualified as Hspec

attachCampaign :: TestGame -> CampaignMode -> Int -> [CampaignRecord] -> IO ()
attachCampaign tg mode completed records = inGame tg do
  let identities = take (length tg.seats) ["01001a", "01010a"]
      initial = (newCampaignOf NextEvolution mode identities) {completed}
  withGame_ $ campaignL ?~ foldl' (flip recordInLog) initial records

campaignNow :: GameAppT Campaign
campaignNow = getsGame (fromMaybe (error "missing campaign") . gameCampaign)

logNow :: GameAppT NextEvolutionLog
logNow = (.campaignLog.nextEvolution) <$> campaignNow

isCampaignCard :: CardCode -> Choice -> Bool
isCampaignCard code = \case CardLabel candidate _ -> code == candidate; _ -> False

spec :: Spec
spec = describe "Next Evolution campaign runtime" do
  it "plays the five scenarios in their printed order" do
    campaignScenarios NextEvolution `Hspec.shouldBe` ["40077", "40103", "40118", "40136", "40163"]

  it "retains the failed scenario's chosen scheme without keeping unearned environments" do
    let before = newCampaignOf NextEvolution StandardCampaign ["01001a"]
        failed = recordInLog (RecordNextEvolutionEnvironment "40190b")
          $ recordInLog (RecordNextEvolutionScheme 1 "40190a") before
        retry = retainNextEvolutionRetryChoice before failed
    retry.campaignLog.nextEvolution.chosenSchemes `Hspec.shouldBe` [(1,"40190a")]
    retry.campaignLog.nextEvolution.earnedEnvironments `Hspec.shouldBe` []
    retry.completed `Hspec.shouldBe` 0

  it "keeps earned environments distinct and tracks Hope's damage separately for each scenario" do
    let initial = newCampaignOf NextEvolution StandardCampaign ["01001a"]
        logged = foldl' (flip recordInLog) initial
          [RecordNextEvolutionEnvironment "40190b",RecordNextEvolutionEnvironment "40190b"
          ,RecordNextEvolutionHopeDamage 3 2,RecordNextEvolutionHopeDamage 4 1]
    logged.campaignLog.nextEvolution.earnedEnvironments `Hspec.shouldBe` ["40190b"]
    logged.campaignLog.nextEvolution.hopeDamage `Hspec.shouldBe` [(3,2),(4,1)]

  it "offers all six campaign schemes in the first scenario and logs the selected one" do
    tg <- testGame ["spider-man"]
    attachCampaign tg StandardCampaign 0 []
    inGame tg do
      runNow [CampaignStep $ CampaignSetupThen "nextEvolution:chooseScheme"]
      for_ ["40190a","40191a","40192a","40193a","40194a","40195a"] \code ->
        offers (isCampaignCard code) `shouldReturn` True
      choose "choose Assemble the Team" (isCampaignCard "40190a")
      (.chosenSchemes) <$> logNow `shouldReturn` [(1,"40190a")]
      schemes <- getSideSchemesInPlay
      liftIO $ map fst schemes `shouldSatisfy` elem "40190a"
      encounters <- getEncounterDeck
      liftIO $ map toCardCode encounters `shouldSatisfy` elem "40199"

  it "reoffers only the original choice when replaying a lost scenario" do
    tg <- testGame ["spider-man"]
    attachCampaign tg StandardCampaign 0 [RecordNextEvolutionScheme 1 "40193a"]
    inGame tg do
      runNow [CampaignStep $ CampaignSetupThen "nextEvolution:chooseScheme"]
      offers (isCampaignCard "40193a") `shouldReturn` True
      for_ ["40190a","40191a","40192a","40194a","40195a"] \code ->
        offers (isCampaignCard code) `shouldReturn` False

  it "does not offer an earlier chosen but undefeated scheme in a later scenario" do
    tg <- testGame ["spider-man"]
    attachCampaign tg StandardCampaign 1 [RecordNextEvolutionScheme 1 "40190a"]
    inGame tg do
      runNow [CampaignStep $ CampaignSetupThen "nextEvolution:chooseScheme"]
      offers (isCampaignCard "40190a") `shouldReturn` False
      offers (isCampaignCard "40191a") `shouldReturn` True

  it "lets the group choose the order of earned environments" do
    tg <- testGame ["spider-man"]
    attachCampaign tg StandardCampaign 2
      [RecordNextEvolutionEnvironment "40190b",RecordNextEvolutionEnvironment "40195b"]
    inGame tg do
      runNow [CampaignStep $ CampaignSetupThen "nextEvolution:environments"]
      offers (isCampaignCard "40190b") `shouldReturn` True
      offers (isCampaignCard "40195b") `shouldReturn` True
      choose "prepare defenses first" (isCampaignCard "40195b")
      selectAny (EnvironmentIs Environments.ne40195b) `shouldReturn` True
      offers (isCampaignCard "40190b") `shouldReturn` True
      offers (isCampaignCard "40195b") `shouldReturn` False

  it "records an environment present at victory and caps expert HP at the printed value" do
    tg <- testGame ["spider-man"]
    attachCampaign tg ExpertCampaign 1 [RecordNextEvolutionScheme 2 "40194a"]
    inGame tg do
      env <- encounterCard Environments.ne40194b
      resolve [PutEncounterCardIntoPlay (player1 tg) env]
      setDamage (player1 tg) 3
      runNow [CampaignStep CampaignVictory]
      choose "continue victory" (isLabel "$label.continue")
      (.earnedEnvironments) <$> logNow `shouldReturn` ["40194b"]
      campaign <- campaignNow
      liftIO $ campaign.completed `shouldBe` 2
      liftIO $ (campaignPlayer "01001a" campaign >>= (.hitPoints)) `shouldBe` Just 7
