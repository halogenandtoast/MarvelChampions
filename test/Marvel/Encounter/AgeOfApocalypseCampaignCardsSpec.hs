module Marvel.Encounter.AgeOfApocalypseCampaignCardsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.AgeOfApocalypse.MissionTypes
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Support.Cards qualified as Supports
import Marvel.Support.Types qualified as Support
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import TestImport

mission :: GameAppT MissionState
mission = getMission >>= maybe (error "no mission") pure

callback :: ToJSON a => Text -> a -> GameAppT ()
callback key value = resolve [CardSpecific GameRef key (toJSON value)]

beginMission :: CardCode -> GameAppT ()
beginMission code = callback "aoaMissionSetup" (code,"45179a" :: CardCode)

completeMission :: GameAppT ()
completeMission = do
  m <- mission
  messages <- L.runQueueT $ L.capture $ for_ m.missionMinions $ \mid -> L.dealDamage mid (SideSchemeRef m.missionScheme) (toDamage 5 FromAbility)
  resolve messages
  resolve [SideSchemeMessage m.missionScheme (SideSchemeThreatRemoved (SideSchemeRef m.missionScheme) 100)]

putDeckTop :: IdentityId -> CardCode -> GameAppT PlayerCard
putDeckTop you code = do
  card <- putInHand you code
  withGame_ $ entitiesL . playersL . ix you %~ \(PlayerIdentity a) -> PlayerIdentity a
    {playerIdentityDeck = Deck (card : unDeck a.playerIdentityDeck)
    ,playerIdentityHand = Hand (filter ((/= pcCardId card) . pcCardId) $ unHand a.playerIdentityHand)}
  pure card

spec :: Spec
spec = describe "Age of Apocalypse campaign cards" do
  for_ ["45166a","45167a","45168a","45169a","45170a"] $ \front ->
    it ("defeats the mission, removes its Finished card, and flips Mission Team: " <> show front) do
      tg <- testGame ["spider-man"]
      inGame tg do
        beginMission front
        m <- mission
        completeMission
        finished <- mission
        finished.missionFinished `shouldBe` True
        finished.missionSucceeded `shouldBe` True
        getsGame (HashMap.member m.missionScheme . gameSideSchemes) `shouldReturn` False
        teams <- supportsWith "45171b"
        length teams `shouldBe` 1
        victory <- getVictoryDisplay
        map toCardCode victory `shouldSatisfy` elem "45179a"

  it "Liberate the Seattle Core adds one actual set-aside Desperate Measures to each hand" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      beginMission "45166a"
      m <- mission
      messages <- L.runQueueT $ L.capture $ for_ m.missionMinions $ \mid -> L.dealDamage mid (SideSchemeRef m.missionScheme) (toDamage 10 FromAbility)
      resolve messages
      resolve [SideSchemeMessage m.missionScheme (SideSchemeThreatRemoved (SideSchemeRef m.missionScheme) 10)]
      for_ [player1 tg,player2 tg] $ \you -> do
        hand <- handOf you
        length [c | c <- hand, toCardCode c == "45176"] `shouldBe` 1
      cards <- getSetAsidePlayerCards
      map toCardCode cards `shouldSatisfy` notElem "45176"

  it "Evacuate Survivors searches deck and discard for the immediate reward" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45167a"
      before <- length <$> handOf you
      completeMission
      length <$> handOf you >>= (`shouldBe` before + 1)

  it "Sabotage the Sea Wall removes the wall and deals three damage to an enemy" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45168a"
      wall <- revealNow you Schemes.northAmericanSeaWall
      resolve []
      completeMission
      getsGame (HashMap.member (sideSchemeOf wall) . gameSideSchemes) `shouldReturn` False
      villainDamageNow `shouldReturn` 3

  it "Find Lost Mutants gives different physical campaign allies to different players" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      beginMission "45169a"
      m <- mission
      messages <- L.runQueueT $ L.capture $ for_ m.missionMinions $ \mid -> L.dealDamage mid (SideSchemeRef m.missionScheme) (toDamage 10 FromAbility)
      resolve messages
      resolve [SideSchemeMessage m.missionScheme (SideSchemeThreatRemoved (SideSchemeRef m.missionScheme) 10)]
      first <- filter ((`elem` ["45172","45173","45174","45175"]) . toCardCode) <$> handOf (player1 tg)
      second <- filter ((`elem` ["45172","45173","45174","45175"]) . toCardCode) <$> handOf (player2 tg)
      length first `shouldBe` 1
      length second `shouldBe` 1
      map pcCardId first `shouldSatisfy` all (`notElem` map pcCardId second)

  it "failed Evacuate Survivors deals each player one encounter card" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      beginMission "45167a"
      callback "aoaMissionFinish" False
      for_ [player1 tg,player2 tg] $ \you -> length <$> dealtTo you >>= (`shouldBe` 1)

  it "failed Sabotage the Sea Wall finds and reveals the wall" do
    tg <- testGame ["spider-man"]
    inGame tg do
      beginMission "45168a"
      _ <- setEncounterDeck [Schemes.northAmericanSeaWall,Treacheries.hardToKeepDown] []
      callback "aoaMissionFinish" False
      selectAny (SideSchemeIs Schemes.northAmericanSeaWall) `shouldReturn` True

  it "failed Find Lost Mutants discards one card from each hand" do
    tg <- testGame ["spider-man"]
    inGame tg do
      beginMission "45169a"
      before <- length <$> handOf (player1 tg)
      callback "aoaMissionFinish" False
      length <$> handOf (player1 tg) >>= (`shouldBe` before - 1)

  it "a fourth failed Protect the Professor attempt loses the game" do
    tg <- testGame ["spider-man"]
    inGame tg do
      beginMission "45170a"
      callback "aoaMissionFinish" False
      getsGame gameState `shouldReturn` Finished Lost

  it "the finished Mission Team exhausts to let a chosen player draw one" do
    tg <- testGame ["spider-man"]
    inGame tg do
      beginMission "45166a"
      completeMission
      team <- selectJust (SupportNamed $ cdName Supports.missionTeamFinished)
      before <- length <$> handOf (player1 tg)
      choose "finished Mission Team" (isAbilityOf $ SupportRef team)
      choose "draw a card" (isTargetChoice $ player1 tg)
      resolve []
      length <$> handOf (player1 tg) >>= (`shouldBe` before + 1)
      getsGame (fmap (Support.supportExhausted . toAttrs) . HashMap.lookup team . gameSupports) `shouldReturn` Just True
      resolve [RemoveFromPlay $ SupportRef team]
      getsGame (HashMap.member team . gameSupports) `shouldReturn` True

  it "Mission Team follows the first player token" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      beginMission "45166a"
      team <- selectJust (SupportNamed $ cdName Supports.missionTeamFront)
      resolve [PassFirstPlayer]
      getsGame (fmap (Support.supportController . toAttrs) . HashMap.lookup team . gameSupports) `shouldReturn` Just (player2 tg)

  it "North American Sea Wall prevents villain damage until defeated" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      wall <- revealNow you Schemes.northAmericanSeaWall
      resolve []
      sideSchemeThreat (sideSchemeOf wall) `shouldReturn` 4
      vid <- villainIdNow
      getModifiers (VillainRef vid) >>= (`shouldSatisfy` elem CannotTakeDamage)
      resolve [SideSchemeMessage (sideSchemeOf wall) (SideSchemeThwarted (toSource you) 4)]
      getModifiers (VillainRef vid) >>= (`shouldSatisfy` notElem CannotTakeDamage)
      victory <- getVictoryDisplay
      map toCardCode victory `shouldSatisfy` elem "45177"

  it "North American Sea Wall boost deals the physical card as an encounter card" do
    tg <- testGame ["spider-man"]
    inGame tg do
      card <- encounterCard Schemes.northAmericanSeaWall
      vid <- villainIdNow
      resolve [RevealBoostCard card (EnemyVillainId vid)]
      dealt <- dealtTo (player1 tg)
      map ecCardId dealt `shouldSatisfy` elem (ecCardId card)

  it "Panicked Refugees draws a replacement when drawn and is removed by an alter-ego exhaust" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      refugee <- putDeckTop you "45178"
      before <- length <$> handOf you
      messages <- L.runQueueT $ L.capture $ L.drawCards you 1
      resolve messages
      length <$> handOf you >>= (`shouldBe` before + 1)
      let oid = ObligationId $ unCardId $ pcCardId refugee
      getsGame (HashMap.member oid . gameObligations) `shouldReturn` True
      choose "remove Refugees" (isAbilityOf $ ObligationRef oid)
      resolve []
      isExhaustedNow you `shouldReturn` True
      getsGame (HashMap.member oid . gameObligations) `shouldReturn` False

  for_ ["45172","45173","45174","45175"] $ \code ->
    it ("a campaign ally's enters-hand response resolves from hand: " <> show code) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        sid <- mainSchemeIdNow
        messages <- L.runQueueT $ L.capture $ L.placeThreat sid 3
        resolve messages
        clearHand you
        card <- putDeckTop you code
        before <- mainSchemeThreat
        messages' <- L.runQueueT $ L.capture $ L.drawCards you 1
        runNow messages'
        choose "campaign ally response" (\case CardLabel selected _ -> selected == code; _ -> False)
        when (code == "45172") chooseMainScheme
        resolve []
        hand <- handOf you
        map pcCardId hand `shouldSatisfy` elem (pcCardId card)
        case code of
          "45172" -> mainSchemeThreat `shouldReturn` before - 2
          "45173" -> villainDamageNow `shouldReturn` 2
          "45174" -> Villain.villainConfused . toAttrs <$> getVillain `shouldReturn` True
          "45175" -> playerIdentityTough <$> playerAttrs you `shouldReturn` True
          _ -> pure ()

  it "Agent of Apocalypse can activate rather than join the mission" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45166a"
      before <- mainSchemeThreat
      card <- revealNow you Minions.agentOfApocalypse
      choose "Agent activates" (isLabel "$label.aoaAgentActivates")
      resolve []
      mainSchemeThreat `shouldReturn` before + 2
      selectAny (MinionWithId $ minionOf card) `shouldReturn` True

  it "Worldwide Crisis offers three mission threat or damage with surge" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45166a"
      _ <- revealNow you Treacheries.worldwideCrisis
      choose "mission threat" (isLabel "$label.aoaMissionThreeThreat")
      resolve []
      m <- mission
      sideSchemeThreat m.missionScheme `shouldReturn` 8
      _ <- revealNow you Treacheries.worldwideCrisis
      choose "damage and surge" (isLabel "$label.aoaWorldwideCrisisDamage")
      resolve []
      damageOf you `shouldReturn` 1
      length <$> dealtTo you >>= (`shouldBe` 1)

  it "Agent's boost still deals an extra boost when the mission has no ally" do
    tg <- testGame ["spider-man"]
    inGame tg do
      beginMission "45166a"
      vid <- villainIdNow
      card <- encounterCard Minions.agentOfApocalypse
      resolve [RevealBoostCard card (EnemyVillainId vid)]
      Villain.villainBoostCards . toAttrs <$> getVillain >>= (\cards -> length cards `shouldBe` 1)
