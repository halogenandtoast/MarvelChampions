module Marvel.Campaign.AgentsOfShieldSpec (spec) where
import Marvel.Campaign
import Data.List qualified as List
import Marvel.AgentsOfShield.Board qualified as Board
import Marvel.Matchers
import Marvel.Query
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Campaign.AgentsOfShield
import Marvel.Message.Lifted qualified as L
import TestImport
import Test.Hspec qualified as H

spec :: Spec
spec = describe "Agents of SHIELD campaign" do
  it "plays the five missions in printed order" do
    campaignScenarios AgentsOfShieldCampaign `H.shouldBe` ["50064","50086a","50103a","50129a","50165a"]

  it "assigns all 27 evidence combinations evenly to the three board members" do
    let combinations = [[a,b,c] | a <- evidenceGroups!!0,b <- evidenceGroups!!1,c <- evidenceGroups!!2]
    for_ boardCodes $ \board -> length (filter ((==board) . moleFor) combinations) `H.shouldBe` 9
    moleFor ["50185","50188","50191"] `H.shouldBe` "50181a"
    moleFor ["50185","50189","50192"] `H.shouldBe` "50182a"
    moleFor ["50187","50190","50193"] `H.shouldBe` "50183a"

  it "counts the wrong board member as a fourth accusation guess" do
    accusationMistakes ["50185","50188","50191"] ["50185","50188","50191"] `H.shouldBe` 0
    accusationMistakes ["50185","50188","50191"] ["50185","50188","50193"] `H.shouldBe` 2

  it "retains the hidden evidence in saved state but redacts both unrevealed envelopes" do
    let log = noShieldLog {hiddenEvidence=["50185","50189","50193"],evidenceEnvelope=["50186"],earnedEvidence=["50187"]}
        campaign = recordInLog (RecordShieldLog log) $ newCampaignOf AgentsOfShieldCampaign StandardCampaign ["01001a"]
    (fromJSON $ toJSON campaign :: Result Campaign) `H.shouldBe` Success campaign
    (redactCampaign campaign).campaignLog.shield.hiddenEvidence `H.shouldBe` []
    (redactCampaign campaign).campaignLog.shield.evidenceEnvelope `H.shouldBe` []
    (redactCampaign campaign).campaignLog.shield.earnedEvidence `H.shouldBe` ["50187"]

  it "prepares exactly one secret per category and gives evidence without replacing it" do
    tg <- testGame ["spider-man"]
    inGame tg do
      L.runQueueT (L.capture prepareEvidence) >>= start
      pass
      log <- getShieldLog
      length log.hiddenEvidence `shouldBe` 3
      length log.evidenceEnvelope `shouldBe` 6
      for_ evidenceGroups $ \group -> length (filter (`elem` group) log.hiddenEvidence) `shouldBe` 1
      L.runQueueT (L.capture $ gainEvidence 2) >>= start
      pass
      next <- getShieldLog
      length next.earnedEvidence `shouldBe` 2
      length next.evidenceEnvelope `shouldBe` 4
      next.hiddenEvidence `shouldBe` log.hiddenEvidence

  it "does not duplicate evidence when preparation is resumed from a saved envelope" do
    tg <- testGame ["spider-man"]
    inGame tg do
      shieldDo prepareEvidence
      before <- getShieldLog
      shieldDo prepareEvidence
      getShieldLog `shouldReturn` before
      shieldDo $ gainEvidence 99
      final <- getShieldLog
      length final.earnedEvidence `shouldBe` 6
      final.evidenceEnvelope `shouldBe` []
      final.hiddenEvidence `shouldBe` before.hiddenEvidence
      shieldDo $ gainEvidence 1
      getShieldLog `shouldReturn` final

  it "starts each board member with two secrets in the first mission" do
    tg <- testGame ["spider-man"]
    inGame tg do
      revealBoards (player1 tg)
      shieldDo $ runShieldStep (shieldCampaign StandardCampaign 0) (CampaignSetupThen "shield:secrets")
      members <- Board.boardMembers
      length members `shouldBe` 3
      traverse getCountersOn members `shouldReturn` [2,2,2]

  it "restores recorded counters and betrayed sides for later missions" do
    tg <- testGame ["spider-man"]
    inGame tg do
      revealBoards (player1 tg)
      shieldDo $ save noShieldLog {boardSecrets=[("50181a",1),("50182a",2),("50183a",3)],betrayedBoard=["50182a"]}
      shieldDo $ runShieldStep (shieldCampaign StandardCampaign 3) (CampaignSetupThen "shield:secrets")
      for_ [("50181a",1),("50182a",2),("50183a",3)] $ \(code,n) -> countersFor code `shouldReturn` n
      selectAny (EnvironmentIs $ encounterDef "50182a") `shouldReturn` False
      selectAny (AttachmentIs $ encounterDef "50182b") `shouldReturn` True

  it "flips a board member without losing its physical secret counters" do
    tg <- testGame ["spider-man"]
    inGame tg do
      revealBoards (player1 tg)
      eid <- selectJust $ EnvironmentIs $ encounterDef "50181a"
      shieldDo $ L.placeCounters eid 2
      shieldDo $ Board.flipBoardMember eid
      countersFor "50181a" `shouldReturn` 2
      selectAny (EnvironmentIs $ encounterDef "50181a") `shouldReturn` False
      selectAny (AttachmentIs $ encounterDef "50181b") `shouldReturn` True

  it "records Black Widow's remaining minions and side schemes for the next alert level" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let c = shieldCampaign StandardCampaign 0
      withGame_ $ campaignL ?~ c
      _ <- addMinion (player1 tg) Minions.hydraSoldier
      _ <- revealNow (player1 tg) Schemes.bombScare
      pass
      shieldDo $ runShieldStep c CampaignVictory
      log <- getShieldLog
      log.widowRemaining `shouldBe` 2

  it "carries Black Widow's remaining cards into Batroc's alert level" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let c = recordInLog (RecordShieldLog noShieldLog {widowRemaining=2}) $ shieldCampaign StandardCampaign 1
      withGame_ $ campaignL ?~ c
      _ <- revealNow (player1 tg) $ encounterDef "50090a"
      pass
      shieldDo $ runShieldStep c (CampaignSetupThen "shield:setup")
      pass
      eid <- selectJust $ EnvironmentIs $ encounterDef "50090a"
      getCountersOn eid `shouldReturn` 2

  it "adds MODOK's extra lock counters and subtracts the rescued-captive discount" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let c = recordInLog (RecordShieldLog noShieldLog {rescuedCaptives=1}) $ shieldCampaign StandardCampaign 2
      withGame_ $ campaignL ?~ c
      _ <- revealNow (player1 tg) $ encounterDef "50105a"
      pass
      eid <- selectJust $ EnvironmentIs $ encounterDef "50105a"
      getCountersOn eid `shouldReturn` 2
      shieldDo $ runShieldStep c (CampaignSetupThen "shield:setup")
      pass
      getCountersOn eid `shouldReturn` 4

  it "records empty-board evidence rewards once for a victory and preserves betrayed sides" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let c = shieldCampaign StandardCampaign 3
      withGame_ $ campaignL ?~ c
      shieldDo prepareEvidence
      revealBoards (player1 tg)
      eid <- selectJust $ EnvironmentIs $ encounterDef "50182a"
      shieldDo $ L.placeCounters eid 2 >> Board.flipBoardMember eid
      shieldDo $ runShieldStep c CampaignVictory
      log <- getShieldLog
      log.evidenceVictories `shouldBe` 1
      length log.earnedEvidence `shouldBe` 1
      log.betrayedBoard `shouldBe` ["50182a"]
      List.lookup "50182a" log.boardSecrets `shouldBe` Just 2

  it "adds no secrets for a fully correct accusation" do
    tg <- testGame ["spider-man"]
    inGame tg do
      revealBoards (player1 tg)
      shieldDo $ save noShieldLog {hiddenEvidence=["50185","50188","50191"],accusation=["50185","50188","50191"]}
      shieldDo $ runShieldGameStep "aosCompareAccusation" Null
      for_ boardCodes $ \code -> countersFor code `shouldReturn` 0

  it "penalizes both a wrong evidence guess and a wrong accused board member" do
    tg <- testGame ["spider-man"]
    inGame tg do
      revealBoards (player1 tg)
      let guess = ["50185","50188","50193"]
      shieldDo $ save noShieldLog {hiddenEvidence=["50185","50188","50191"],accusation=guess}
      shieldDo $ runShieldGameStep "aosCompareAccusation" Null
      for_ boardCodes $ \code -> countersFor code `shouldReturn` (if code == moleFor guess then 5 else 2)
      selectAny (AttachmentIs $ encounterDef $ if moleFor guess == "50182a" then "50182b" else "50183b") `shouldReturn` True

  for_ [False,True] $ \alreadyBetrayed ->
    it (if alreadyBetrayed then "adds the accusation penalty to an already betrayed member" else "adds the accusation penalty after global secrets flip its target") do
      tg <- testGame ["spider-man"]
      inGame tg do
        revealBoards (player1 tg)
        let guess = ["50185","50188","50193"]
            accused = moleFor guess
        eid <- selectJust $ EnvironmentIs $ encounterDef accused
        shieldDo $ L.placeCounters eid 2
        when alreadyBetrayed $ shieldDo $ Board.flipBoardMember eid
        shieldDo $ save noShieldLog {hiddenEvidence=["50185","50188","50191"],accusation=guess}
        shieldDo $ runShieldGameStep "aosCompareAccusation" Null
        countersFor accused `shouldReturn` 7
        selectAny (AttachmentIs $ encounterDef $ if accused == "50182a" then "50182b" else "50183b") `shouldReturn` True

  it "reveals the actual mole even when the players accused another member" do
    tg <- testGame ["spider-man"]
    inGame tg do
      revealBoards (player1 tg)
      shieldDo $ save noShieldLog {hiddenEvidence=["50185","50188","50191"],accusation=["50185","50188","50193"]}
      shieldDo $ runShieldGameStep "aosRevealMole" Null
      selectAny (AttachmentIs $ encounterDef "50181b") `shouldReturn` True
      selectAny (EnvironmentIs $ encounterDef "50182a") `shouldReturn` True

  it "allows a player to decline an earned evidence card and its starting-hand search" do
    tg <- testGame ["spider-man"]
    inGame tg do
      revealBoards (player1 tg)
      before <- handOf (player1 tg)
      shieldDo $ evidenceSetup "50185"
      choose "decline extra card" (isLabel "$label.done")
      choose "decline search" (isLabel "$label.done")
      pass
      handOf (player1 tg) `shouldReturn` before
      for_ boardCodes $ \code -> countersFor code `shouldReturn` 0

  it "buys a matching evidence ally and searches it into the starting hand for main-scheme threat" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      clearHand p
      revealBoards p
      eid <- selectJust $ EnvironmentIs $ encounterDef "50181a"
      beforeThreat <- mainSchemeThreatNow
      shieldDo $ evidenceSetup "50185"
      choose "pay a board secret" (isTargetChoice eid)
      choose "Luke Cage" (\case CardLabel code _ -> code == "01076"; _ -> False)
      cards <- unDeck . playerIdentityDeck . toAttrs <$> getPlayer p
      let luke = fromMaybe (error "evidence ally missing from deck") $ headMay (filter ((== "01076") . toCardCode) cards)
      choose "search for the ally" (isLabel "$label.aosEvidenceSearch")
      choose "draw the evidence ally" (isTargetChoice luke)
      pass
      map toCardCode <$> handOf p >>= (`shouldSatisfy` elem "01076")
      mainSchemeThreatNow `shouldReturn` (beforeThreat + 1)
      countersFor "50181a" `shouldReturn` 1

  it "clears the final mission's envelopes on a standard retry while retaining earned-victory count" do
    let before = recordInLog (RecordShieldLog noShieldLog {hiddenEvidence=["50185","50188","50191"],evidenceEnvelope=["50186"],earnedEvidence=["50187"],accusation=["50185","50188","50193"],evidenceVictories=3}) $ shieldCampaign StandardCampaign 4
        retry = retainNextEvolutionRetryChoice before before
    retry.completed `H.shouldBe` 4
    retry.campaignLog.shield.hiddenEvidence `H.shouldBe` []
    retry.campaignLog.shield.evidenceEnvelope `H.shouldBe` []
    retry.campaignLog.shield.earnedEvidence `H.shouldBe` []
    retry.campaignLog.shield.accusation `H.shouldBe` []
    retry.campaignLog.shield.evidenceVictories `H.shouldBe` 3

  it "retains the evidence envelopes on an earlier expert retry and discards unearned new rewards" do
    let originalLog = noShieldLog {hiddenEvidence=["50185","50188","50191"],evidenceEnvelope=["50186","50187"],earnedEvidence=["50189"]}
        before = recordInLog (RecordShieldLog originalLog) $ shieldCampaign ExpertCampaign 2
        current = recordInLog (RecordShieldLog originalLog {earnedEvidence=["50189","50186"],evidenceEnvelope=["50187"],evidenceVictories=9}) before
    (retainNextEvolutionRetryChoice before current).campaignLog.shield `H.shouldBe` originalLog

  it "lets an expert player keep recorded hit points without buying recovery" do
    tg <- testGame ["spider-man"]
    inGame tg do
      revealBoards (player1 tg)
      let c = recordInLog (RecordHitPoints "01001a" 3) $ shieldCampaign ExpertCampaign 1
      shieldDo $ expertSetup c
      choose "keep hit points" (isLabel "$label.keepHitPoints")
      pass
      damageOf (player1 tg) `shouldReturn` 7
      for_ boardCodes $ \code -> countersFor code `shouldReturn` 0

  it "lets an expert player add a board secret to heal their printed REC" do
    tg <- testGame ["spider-man"]
    inGame tg do
      revealBoards (player1 tg)
      let c = recordInLog (RecordHitPoints "01001a" 3) $ shieldCampaign ExpertCampaign 1
      eid <- selectJust $ EnvironmentIs $ encounterDef "50181a"
      shieldDo $ expertSetup c
      choose "buy recovery from the board" (isTargetChoice eid)
      pass
      damageOf (player1 tg) `shouldReturn` 4
      countersFor "50181a" `shouldReturn` 1

shieldCampaign :: CampaignMode -> Int -> Campaign
shieldCampaign mode completed = (newCampaignOf AgentsOfShieldCampaign mode ["01001a"]) {completed}

shieldDo :: L.QueueT GameAppT () -> GameAppT ()
shieldDo action = L.runQueueT (L.capture action) >>= runNow

revealBoards :: IdentityId -> GameAppT ()
revealBoards you = for_ boardCodes $ \code -> revealNow you (encounterDef code) >> pass

countersFor :: CardCode -> GameAppT Natural
countersFor code = do
  members <- Board.boardMembers
  matches <- filterM (fmap (== Just code) . boardCode) members
  sum <$> traverse getCountersOn matches
