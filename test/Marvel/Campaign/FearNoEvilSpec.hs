module Marvel.Campaign.FearNoEvilSpec (spec) where

import Data.List qualified as List
import Data.Text qualified as Text
import Marvel.Campaign
import Marvel.CampaignPlayers.Helpers qualified as Stored
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Query
import Test.Hspec qualified as H
import TestImport

freshCampaign :: CampaignMode -> Campaign
freshCampaign mode = newCampaignOf FearNoEvilCampaign mode ["01001a"]

withLog :: FearNoEvilLog -> Campaign -> Campaign
withLog log c = c {campaignLog=c.campaignLog {fearNoEvil=log}}

campaignSandbox :: Text -> IO TestGame
campaignSandbox hero = do
  tg <- testGameAgainst "60134a" [hero]
  let c = recordInLog (RecordFearNoEvilScenario "60134a")
        $ newCampaignOf FearNoEvilCampaign StandardCampaign [(.hero) $ starter hero]
  tg <$ inGame tg (withGame_ $ campaignL ?~ c)

removedTitlesNow :: GameAppT [Text]
removedTitlesNow = getsGame $ maybe [] (.campaignLog.fearNoEvil.removedTitles) . gameCampaign

spec :: Spec
spec = describe "Fear No Evil campaign" do
  it "explains the campaign and expert rules before choosing the villain order" do
    tg <- campaignSandbox "spider-man"
    inGame tg do
      withGame_ $ campaignL ?~ freshCampaign ExpertCampaign
      runNow [CampaignStep CampaignChoices]
      currentReading `shouldReturn`
        ["$story.fearNoEvil.intro","$story.fearNoEvil.campaignMode","$story.fearNoEvil.expertCampaign","$story.fearNoEvil.planning"]
      campaign <- getsGame $ fromMaybe (error "campaign missing") . gameCampaign
      campaign.campaignLog.fearNoEvil.progress `shouldBe` []
      choose "continue the campaign introduction" (isLabel "$label.continue")
      offers (isLabel "$label.fneRecommendedVillains") `shouldReturn` True

  it "briefs the selected scenario and its actual assigned underling" do
    tg <- campaignSandbox "spider-man"
    inGame tg do
      let c = recordInLog (RecordFearNoEvilUnderling "60121a" "60065")
            $ recordInLog (RecordFearNoEvilScenario "60121a") $ freshCampaign StandardCampaign
      withGame_ $ campaignL ?~ c
      runNow [CampaignStep CampaignSetup]
      currentReading `shouldReturn`
        ["$story.fearNoEvil.artMuseumHeist.intro","$story.fearNoEvil.artMuseumHeist.rules","$story.fearNoEvil.underlings.bullseye","$story.fearNoEvil.campaignSetup","$story.fearNoEvil.artMuseumHeist.setup"]

  it "shows victory instructions before recording the scenario as completed" do
    tg <- campaignSandbox "spider-man"
    inGame tg do
      runNow [GameOver Won]
      currentReading `shouldReturn` ["$story.fearNoEvil.protectionRacket.victory"]
      before <- getsGame $ fromMaybe (error "campaign missing") . gameCampaign
      before.campaignLog.fearNoEvil.resolved `shouldBe` []
      choose "continue after the victory instructions" (isLabel "$label.continue")
      after <- getsGame $ fromMaybe (error "campaign missing") . gameCampaign
      List.lookup "60134a" after.campaignLog.fearNoEvil.resolved `shouldBe` Just True
      getsGame gameState `shouldReturn` Finished Won

  it "can read the defeat instructions after the final identity has left play" do
    tg <- campaignSandbox "spider-man"
    inGame tg do
      runNow [RemoveFromPlay $ IdentityRef $ player1 tg]
      currentReading `shouldReturn` ["$story.fearNoEvil.protectionRacket.defeat"]
      choose "continue after the defeat instructions" (isLabel "$label.continue")
      getsGame gameState `shouldReturn` Finished Lost

  it "announces a defeat only once after recording the campaign outcome" do
    tg <- campaignSandbox "spider-man"
    inGame tg do
      runNow [GameOver Lost]
      currentReading `shouldReturn` ["$story.fearNoEvil.protectionRacket.defeat"]
      choose "continue after the defeat instructions" (isLabel "$label.continue")
      getsGame gameState `shouldReturn` Finished Lost
      entries <- liftIO $ readIORef tg.logged
      length (filter (Text.isInfixOf "$log.gameOver.lost" . entryText) entries) `shouldBe` 1
      campaign <- getsGame $ fromMaybe (error "campaign missing") . gameCampaign
      campaign.campaignLog.fearNoEvil.selectedScenario `shouldBe` Nothing

  it "offers the expert healing choice and heals only REC for one facedown encounter card" do
    tg <- campaignSandbox "spider-man"
    inGame tg do
      let you = player1 tg
      resolve =<< L.runQueueT (L.capture $ L.setHitPoints you 6)
      _ <- setEncounterDeck [Cards.street60188,Cards.street60188] []
      runNow [CampaignStep $ CampaignSetupThen "fearNoEvil:heal"]
      choose "take the encounter card" (isLabel "$label.fneEncounterToHealRecovery")
      pass
      damageOf you `shouldReturn` 1
      length . playerIdentityEncounterCards . toAttrs <$> getPlayer you `shouldReturn` 1

  it "allows declining expert healing without dealing an encounter card" do
    tg <- campaignSandbox "spider-man"
    inGame tg do
      let you = player1 tg
      resolve =<< L.runQueueT (L.capture $ L.setHitPoints you 6)
      runNow [CampaignStep $ CampaignSetupThen "fearNoEvil:heal"]
      choose "keep the recorded hit points" (isLabel "$label.keepHitPoints")
      pass
      damageOf you `shouldReturn` 4
      length . playerIdentityEncounterCards . toAttrs <$> getPlayer you `shouldReturn` 0

  it "rejoins a selected defeated hero by healing their REC and dealing one encounter card" do
    tg <- campaignSandbox "spider-man"
    inGame tg do
      let you = player1 tg
      resolve =<< L.runQueueT (L.capture $ L.setHitPoints you 0)
      _ <- setEncounterDeck [Cards.street60188,Cards.street60188] []
      resolve [CampaignStep $ CampaignSetupThen "fearNoEvil:heal"]
      pass
      damageOf you `shouldReturn` 7
      length . playerIdentityEncounterCards . toAttrs <$> getPlayer you `shouldReturn` 1

  it "records expert remaining hit points after a defeat as well as after a victory" do
    tg <- campaignSandbox "spider-man"
    inGame tg do
      let you = player1 tg
          c = recordInLog (RecordFearNoEvilScenario "60134a") $ freshCampaign ExpertCampaign
      withGame_ $ campaignL ?~ c
      resolve =<< L.runQueueT (L.capture $ L.setHitPoints you 4)
      resolve [CampaignStep $ CampaignSetupThen "fearNoEvil:defeat"]
      result <- getsGame $ fromMaybe (error "campaign missing") . gameCampaign
      (campaignPlayer "01001a" result >>= (.hitPoints)) `shouldBe` Just 4

  it "removes every printing of a recorded title from the next deck" do
    let c = withLog (noFearNoEvilLog {removedTitles=["Nick Fury"]}) $ freshCampaign StandardCampaign
        deck = (starter "spider-man") {cards=[("01084",1),("08019",1),("50002",1),("01088",1)]}
    (campaignDeck c deck).cards `H.shouldBe` [("01088",1)]

  it "records a removed unique support at victory" do
    tg <- campaignSandbox "spider-man"
    inGame tg do
      support <- putIntoPlay (player1 tg) "01006"
      resolve =<< L.runQueueT (L.capture $ L.removeFromGame $ supportIdOf support)
      resolve [GameOver Won]
      removedTitlesNow `shouldReturn` ["Aunt May"]

  it "records a removed unique ally even when the ally was temporarily treated as a minion" do
    tg <- campaignSandbox "spider-man"
    inGame tg do
      let you = player1 tg
      ally <- putIntoPlay you "01002"
      resolve =<< L.runQueueT (L.capture $ L.allyBecomesMinion $ allyIdOf ally)
      let mid = MinionId $ unAllyId $ allyIdOf ally
      physical <- getsGame $ \game -> cardInPlay game (MinionRef mid)
      (cdCardType . getCardDef <$> physical) `shouldBe` Just AllyType
      resolve =<< L.runQueueT (L.capture $ L.removeFromGame mid)
      selectAny (MinionWithId mid) `shouldReturn` False
      fearNoEvilRemovedTitles `shouldReturn` ["Black Cat"]
      resolve [GameOver Won]
      removedTitlesNow `shouldReturn` ["Black Cat"]

  it "does not record a printed encounter minion as a campaign ally when it was removed under player control" do
    tg <- campaignSandbox "spider-man"
    inGame tg do
      let you = player1 tg
      mid <- addMinion you Cards.street60169
      resolve =<< L.runQueueT (L.capture do
        L.placeCharacterThreat mid 4
        L.takeControlOfMinionWithConsequential you mid 2)
      resolve =<< L.runQueueT (L.capture $ L.removeFromGame $ AllyId $ unMinionId mid)
      resolve [GameOver Won]
      removedTitlesNow `shouldReturn` []

  it "does not ban a unique ally temporarily moved beneath a support" do
    tg <- campaignSandbox "spider-man"
    inGame tg do
      let you = player1 tg
      support <- putIntoPlay you "01091"
      ally <- putInHand you "01084"
      resolve =<< L.runQueueT (L.capture $ L.tuckPlayerCardUnder (supportIdOf support) ally)
      resolve [GameOver Won]
      removedTitlesNow `shouldReturn` []

  it "does not ban a unique ally still stored beneath Echo's identity at victory" do
    tg <- campaignSandbox "echo"
    inGame tg do
      let you = player1 tg
      ally <- putInHand you "01084"
      resolve =<< L.runQueueT (L.capture $ Stored.tuckPlayerCard you ally)
      Stored.tuckedPlayerCards you >>= (liftIO . (`shouldContain` [ally]))
      resolve [GameOver Won]
      removedTitlesNow `shouldReturn` []

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

currentReading :: GameAppT [Text]
currentReading = openQuestion >>= \case
  Just (_,Read sections _) -> pure sections
  _ -> error "Fear No Evil campaign reading was not asked"
