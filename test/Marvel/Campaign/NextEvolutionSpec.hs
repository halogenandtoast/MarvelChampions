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
  for_ [StandardCampaign, ExpertCampaign] $ \mode ->
    for_ (zip [0..] ["morlockSiege", "onTheRun", "juggernaut", "misterSinister", "stryfe"]) $ \(completed, scenarioSlug) ->
      it ("reads the scenario introduction and applicable guide rules before setup: " <> show (completed + 1) <> " " <> show mode) do
        tg <- testGame ["spider-man"]
        attachCampaign tg mode completed []
        inGame tg do
          runNow [CampaignStep CampaignChoices]
          let key section = "$story.nextEvolution." <> scenarioSlug <> "." <> section
              expected = [key "intro"]
                <> ["$story.nextEvolution." <> section | completed == 0, section <- ["campaignMode", "campaignSchemes", "playerSideSchemes", "keywords"]]
                <> ["$story.nextEvolution.expertCampaign" | completed == 0 && mode == ExpertCampaign]
                <> ["$story.nextEvolution.hopeSummers" | completed == 2]
                <> [key "rules",key "setup"]
                <> [key "expertSetup" | completed > 0 && mode == ExpertCampaign]
          openQuestion >>= \case
            Just (_,Read sections _) -> sections `shouldBe` expected
            question -> error $ "expected campaign guide reading: " <> show question
          (.chosenSchemes) <$> logNow `shouldReturn` []
          campaign <- campaignNow
          campaign.completed `shouldBe` completed

  it "reads the campaign conclusion before recording the final victory" do
    tg <- testGame ["spider-man"]
    attachCampaign tg StandardCampaign 4 []
    inGame tg do
      runNow [CampaignStep CampaignVictory]
      openQuestion >>= \case
        Just (_,Read sections _) -> sections `shouldBe` ["$story.nextEvolution.stryfe.victory", "$story.nextEvolution.conclusion"]
        question -> error $ "expected final campaign reading: " <> show question
      campaign <- campaignNow
      campaign.completed `shouldBe` 4
      choose "read the conclusion" (isLabel "$label.continue")
      (.completed) <$> campaignNow `shouldReturn` 5

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
