module Marvel.Campaign.FearNoEvilCardsSpec (spec) where

import Marvel.Campaign qualified as C
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Modifier
import Marvel.Query
import Marvel.Trait qualified as Trait
import TestImport

campaignFixture :: Difficulty -> [Text] -> IO TestGame
campaignFixture mode heroes = do
  let setup = (fromMaybe (error "Rhino setup missing") $ recommendedSetup "01094") {difficulty=mode}
  (app,ran,logged) <- dealtGameWith id setup (map starter heroes) 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  tg <$ inGame tg toFirstTurn

environmentInPlay :: IdentityId -> CardDef -> GameAppT EnvironmentId
environmentInPlay you def = do
  card <- encounterCard def
  resolve =<< L.runQueueT (L.capture $ L.putEncounterCardIntoPlay you card)
  pure $ EnvironmentId $ unCardId $ ecCardId card

setupEnvironment :: EnvironmentId -> GameAppT [Message]
setupEnvironment eid = L.runQueueT $ L.capture $ L.cardSpecific eid "campaignEnvironmentSetup" ()

cardCodeChoice :: CardCode -> Choice -> Bool
cardCodeChoice code = \case CardLabel actual _ -> actual == code; _ -> False

spec :: Spec
spec = describe "Fear No Evil campaign card effects" do
  it "completed Art Museum Heist gives each identity tough" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      eid <- environmentInPlay (player1 tg) Cards.street60205a
      resolve =<< setupEnvironment eid
      for_ [player1 tg,player2 tg] $ \you ->
        getCharacterStatusCount (IdentityCharacter you) ToughStatus `shouldReturn` 1

  it "failed Art Museum Heist lets each normal-mode player choose a different status" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      eid <- environmentInPlay (player1 tg) Cards.street60205b
      runNow =<< setupEnvironment eid
      choose "confuse the first identity" (isLabel "$label.fneBeConfused")
      choose "stun the second identity" (isLabel "$label.fneBeStunned")
      pass
      getCharacterStatusCount (IdentityCharacter $ player1 tg) ConfusedStatus `shouldReturn` 1
      getCharacterStatusCount (IdentityCharacter $ player1 tg) StunnedStatus `shouldReturn` 0
      getCharacterStatusCount (IdentityCharacter $ player2 tg) StunnedStatus `shouldReturn` 1

  it "failed Art Museum Heist gives both statuses in expert mode" do
    tg <- campaignFixture Expert ["spider-man"]
    inGame tg do
      eid <- environmentInPlay (player1 tg) Cards.street60205b
      resolve =<< setupEnvironment eid
      getCharacterStatusCount (IdentityCharacter $ player1 tg) ConfusedStatus `shouldReturn` 1
      getCharacterStatusCount (IdentityCharacter $ player1 tg) StunnedStatus `shouldReturn` 1

  for_ [(Cards.street60206a,"01008"),(Cards.street60207a,"01006"),(Cards.street60208a,"01084")] $ \(def,rewardCode) ->
    for_ [Normal,Expert] $ \mode ->
      it ("searches for the " <> show (toCardCode def) <> " reward in " <> show mode <> " mode") do
        tg <- campaignFixture mode ["spider-man"]
        inGame tg do
          let you = player1 tg
          clearHand you
          [reward] <- stackDeck you [rewardCode]
          spare <- putInHand you "01088"
          eid <- environmentInPlay you def
          runNow =<< setupEnvironment eid
          choose "search for the campaign reward" (isLabel "$label.fneSearchCampaignReward")
          choose "take the selected reward" (isTargetChoice reward)
          when (mode == Expert) $ choose "discard the other card" (isTargetChoice spare)
          pass
          handOf you >>= \cards -> liftIO $ map pcCardId cards `shouldContain` [pcCardId reward]
          length <$> handOf you `shouldReturn` (if mode == Expert then 1 else 2)

  for_ [(Normal,1),(Expert,2)] $ \(mode,amount) ->
    it ("failed Protection Racket discards " <> show amount <> " hand cards in " <> show mode <> " mode") do
      tg <- campaignFixture mode ["spider-man"]
      inGame tg do
        let you = player1 tg
        clearHand you
        for_ ["01088","01089","01090"] $ void . putInHand you
        eid <- environmentInPlay you Cards.street60207b
        resolve =<< setupEnvironment eid
        length <$> handOf you `shouldReturn` (3 - amount)

  for_ [(Normal,0,9),(Expert,1,7)] $ \(mode,dealt,deckSize) ->
    it ("failed Raft Breakout distributes all six physical prisoners in " <> show mode <> " mode") do
      tg <- campaignFixture mode ["spider-man","captain-marvel"]
      inGame tg do
        _ <- setEncounterDeck (replicate 3 Cards.street60085) []
        eid <- environmentInPlay (player1 tg) Cards.street60208b
        resolve =<< setupEnvironment eid
        length <$> getEncounterDeck `shouldReturn` deckSize
        for_ [player1 tg,player2 tg] $ \you -> do
          cards <- playerIdentityEncounterCards . toAttrs <$> getPlayer you
          length cards `shouldBe` dealt
          liftIO $ map (cdCardType . getCardDef) cards `shouldSatisfy` all (== MinionType)

  for_ [(Normal,1),(Expert,2)] $ \(mode,amount) ->
    it ("failed Stop the Presses deals " <> show amount <> " encounter cards to each player in " <> show mode <> " mode") do
      tg <- campaignFixture mode ["spider-man","captain-marvel"]
      inGame tg do
        _ <- setEncounterDeck (replicate 8 Cards.street60085) []
        eid <- environmentInPlay (player1 tg) Cards.street60209b
        resolve =<< setupEnvironment eid
        for_ [player1 tg,player2 tg] $ \you ->
          length . playerIdentityEncounterCards . toAttrs <$> getPlayer you `shouldReturn` amount

  it "failed Getaway deals a facedown encounter card when the encounter deck contains no attachment" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      ([first,_,_],_) <- setEncounterDeck (replicate 3 Cards.street60085) []
      eid <- environmentInPlay you Cards.street60206b
      resolve =<< setupEnvironment eid
      map ecCardId . playerIdentityEncounterCards . toAttrs <$> getPlayer you `shouldReturn` [ecCardId first]

  it "failed Getaway's expert discard restriction ends after the first round" do
    tg <- campaignFixture Expert ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- villainIdNow
      card <- encounterCard Cards.street60117
      let aid = AttachmentId $ unCardId $ ecCardId card
      resolve =<< L.runQueueT (L.capture $ L.attachEncounterToCharacter card $ VillainCharacter vid)
      _ <- environmentInPlay you Cards.street60206b
      getModifiers aid >>= (liftIO . (`shouldContain` [CannotBeDiscarded]))
      withGame_ $ \g -> g {gameRound=2}
      getModifiers aid >>= (liftIO . (`shouldNotContain` [CannotBeDiscarded]))

  it "completed Stop the Presses excludes removed and already occupied Daily Bugle supports" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
          log = C.noFearNoEvilLog {C.removedTitles=["Robbie Robertson"]}
          campaign = C.recordInLog (C.RecordFearNoEvilLog log) $ C.newCampaignOf C.FearNoEvilCampaign C.StandardCampaign ["01001a","01010a"]
      withGame_ $ campaignL ?~ campaign
      occupied <- encounterCard Cards.street60155
      resolve =<< L.runQueueT (L.capture $ L.putEncounterCardIntoPlay (player2 tg) occupied)
      cards <- traverse encounterCard [Cards.street60153,Cards.street60154,Cards.street60155,Cards.street60156]
      resolve =<< L.runQueueT (L.capture $ L.setAside $ map EncounterCard cards)
      eid <- environmentInPlay you Cards.street60209a
      runNow =<< setupEnvironment eid
      offers (cardCodeChoice "60156") `shouldReturn` False
      offers (cardCodeChoice "60155") `shouldReturn` False
      choose "take Ben Urich" (cardCodeChoice "60153")
      choose "take Betty Brant" (cardCodeChoice "60154")
      pass
      selectListCount (SupportWithTrait Trait.DailyBugle) `shouldReturn` 3

  it "campaign Mary heals and flips on the matching reveal type while retaining her physical ID" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      card <- encounterCard Cards.street60210a
      let aid = AllyId $ unCardId $ ecCardId card
      resolve =<< L.runQueueT (L.capture $ L.putEncounterCardIntoPlay you card)
      resolve [AllyMessage aid $ AllySetDamage 2]
      _ <- addMinion you Cards.street60092
      bloody <- allyIn "60210b"
      allyId bloody `shouldBe` aid
      allyDamage bloody `shouldBe` 0
      resolve [AllyMessage aid $ AllySetDamage 2]
      scheme <- encounterCard Cards.street60094
      resolve [RevealEncounterCard you scheme]
      typhoid <- allyIn "60210a"
      allyId typhoid `shouldBe` aid
      allyDamage typhoid `shouldBe` 0
