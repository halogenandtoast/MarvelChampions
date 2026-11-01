module Marvel.Campaign.AgeOfApocalypseSpec (spec) where

import TestImport
import Marvel.AgeOfApocalypse.MissionTypes
import Marvel.Campaign

campaignNow :: GameAppT Campaign
campaignNow = getsGame (fromMaybe (error "missing campaign") . gameCampaign)

campaignGame :: CampaignMode -> Int -> IO TestGame
campaignGame mode completed = do
  let base = (newCampaignOf AgeOfApocalypseCampaign mode ["01001a"]) {completed}
      used = zip [1..] (take completed apocalypseMissionCards)
      c = foldl' (flip recordInLog) base [RecordApocalypseMission n code | (n,code) <- used]
      code = fromMaybe (error "campaign finished") (currentScenario c)
      setup = fromMaybe (error "scenario not registered") (recommendedSetup code)
  (app, ran, logged) <- dealtGameWith (campaignL ?~ c) setup [starter "spider-man"] 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

spec :: Spec
spec = describe "Age of Apocalypse campaign runtime" do
  for_ [0..4] $ \completed ->
    it ("sets up scenario " <> show (completed + 1) <> " with a mission and starting ally") do
      tg <- campaignGame StandardCampaign completed
      inGame tg do
        m <- getMission >>= maybe (error "missing mission") pure
        m.missionFinished `shouldBe` False
        length m.missionMinions `shouldBe` 1
        m.missionSettingUp `shouldBe` False
        if completed == 4 then m.missionCard `shouldBe` "45170a"
          else m.missionCard `shouldSatisfy` (`notElem` take completed apocalypseMissionCards)
        hand <- handOf (player1 tg)
        hand `shouldSatisfy` any ((== AllyType) . cdCardType . getCardDef)
        size <- handSizeOf (player1 tg)
        fromIntegral (length hand) `shouldBe` size
        (.campaignLog.apocalypse.missions) <$> campaignNow >>= (`shouldSatisfy` elem (completed + 1,m.missionCard))

  it "records an attempted mission and a win before moving to the next scenario" do
    tg <- campaignGame StandardCampaign 0
    inGame tg do
      m <- getMission >>= maybe (error "missing mission") pure
      resolve [GameOver Won]
      c <- campaignNow
      c.completed `shouldBe` 1
      c.campaignLog.apocalypse.missions `shouldSatisfy` elem (1,m.missionCard)
      currentScenario c `shouldBe` Just "45081a"

  it "loses the campaign when the final scenario is won without protecting the Professor" do
    tg <- campaignGame StandardCampaign 4
    inGame tg do
      resolve [GameOver Won]
      c <- campaignNow
      c.lost `shouldBe` True
      currentScenario c `shouldBe` Nothing

  it "wins the campaign when the Professor mission was completed" do
    tg <- campaignGame StandardCampaign 4
    inGame tg do
      resolve [CardSpecific GameRef "aoaMissionFinish" (toJSON True)]
      resolve [GameOver Won]
      c <- campaignNow
      c.lost `shouldBe` False
      c.completed `shouldBe` 5
      c.campaignLog.apocalypse.successfulMissions `shouldSatisfy` elem "45170a"
      currentScenario c `shouldBe` Nothing

  it "permits retrying an expert final scenario lost during play" do
    tg <- campaignGame ExpertCampaign 4
    inGame tg do
      resolve [GameOver Lost]
      c <- campaignNow
      c.lost `shouldBe` False
      c.completed `shouldBe` 4
      currentScenario c `shouldBe` Just "45184a"
