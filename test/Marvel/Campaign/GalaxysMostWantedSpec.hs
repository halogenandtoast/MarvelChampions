module Marvel.Campaign.GalaxysMostWantedSpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import Data.List qualified as List
import Data.Functor.Identity (Identity (..))
import Marvel.EncounterCard (allEncounterCards)
import Marvel.Matchers
import Marvel.Query
import Marvel.Campaign
import Marvel.SideScheme.Cards qualified as SideSchemes
import TestImport

groot, rocket :: CardCode
groot = "16001a"
rocket = "16029a"

-- | A Galaxy's Most Wanted campaign for Groot with so many scenarios won, the next
-- played against Rhino (the box's own scenarios are not on this branch).
campaignAt :: CampaignMode -> Int -> Campaign
campaignAt mode n = campaignFor mode n [groot]

campaignFor :: CampaignMode -> Int -> [CardCode] -> Campaign
campaignFor mode n heroes = c {scenarios = [if i == n then "01094" else code | (i, code) <- zip [0 ..] c.scenarios], completed = n}
 where
  c = newCampaignOf GalaxysMostWanted mode heroes

rhino :: ScenarioSetup
rhino = fromMaybe (error "no Rhino") (recommendedSetup "01094")

dealCampaignUnread :: Campaign -> [DeckList] -> IO TestGame
dealCampaignUnread c decks = do
  (app, ran, logged) <- dealtGameWith (campaignL ?~ c) rhino decks 1
  seats <- runGameApp app (getsGame gameSeats)
  pure $ TestGame app ran logged seats

-- | Dealt, its story read: its Setup done up to the next question.
dealCampaign :: Campaign -> [DeckList] -> IO TestGame
dealCampaign c decks = do
  tg <- dealCampaignUnread c decks
  tg <$ inGame tg readStory

readStory :: GameAppT [Text]
readStory =
  openQuestion >>= \case
    Just (ident, Read sections _) -> sections <$ answerQuestion ident 0
    q -> error $ "nothing to read: " <> show q

win :: GameAppT ()
win = do
  withGame_ $ questionL .~ mempty
  pushAll [GameOver Won]
  runGameMessages

campaignNow :: GameAppT Campaign
campaignNow = getsGame (fromMaybe (error "no campaign") . gameCampaign)

grootNow :: GameAppT GalaxyPlayer
grootNow = fromMaybe (error "no Groot in the log") . galaxyPlayer groot <$> campaignNow

sideSchemeCodes :: GameAppT [CardCode]
sideSchemeCodes = map fst <$> getSideSchemesInPlay

withUnits :: CardCode -> Natural -> Campaign -> Campaign
withUnits ident n = recordInLog (RecordUnits ident n)

-- | A test of the log alone, outside a game.
check :: String -> IO () -> Spec
check = it

spec :: Spec
spec = describe "The Galaxy's Most Wanted campaign" do
  describe "the campaign" do
    check "loads a campaign saved before there were two as The Rise of Red Skull's, its log's new section empty" do
      let saved = case toJSON (newCampaign StandardCampaign ["01001a"]) of
            Object o -> Object (KeyMap.delete "kind" (KeyMap.alterF (Identity . fmap dropGalaxy) "campaignLog" o & runIdentity))
            v -> v
          dropGalaxy = \case
            Object l -> Object (KeyMap.delete "galaxy" l)
            v -> v
      case fromJSON saved of
        Success (c :: Campaign) -> do
          c.kind `shouldBe` RiseOfRedSkull
          c.scenarios `shouldBe` riseOfRedSkull
          c.campaignLog.galaxy `shouldBe` noGalaxyLog
        Error e -> expectationFailure e

    check "plays its five scenarios by their villains, each player's column of the log empty" do
      let c = newCampaignOf GalaxysMostWanted StandardCampaign [groot, rocket]
      c.scenarios `shouldBe` galaxysMostWanted
      map (.identity) c.campaignLog.galaxy.galaxyPlayers `shouldBe` [groot, rocket]
      map (.units) c.campaignLog.galaxy.galaxyPlayers `shouldBe` [0, 0]

  describe "units and The Market" do
    check "buys a card its unit cost no more than the player's unspent units, subtracting them" do
      let c = withUnits groot 4 (campaignAt StandardCampaign 1)
          bought = recordInLog (BuyMarketCard groot "16158") c
      canBuy groot "16158" c `shouldBe` True
      (.units) <$> galaxyPlayer groot bought `shouldBe` Just 1
      (.marketCards) <$> galaxyPlayer groot bought `shouldBe` Just ["16158"]

    check "does not buy a card that costs more than the units unspent" do
      let c = withUnits groot 4 (campaignAt StandardCampaign 1)
      canBuy groot "16166" c `shouldBe` False
      recordInLog (BuyMarketCard groot "16166") c `shouldBe` c

    check "has one copy of each card for the players as a group" do
      let c = withUnits rocket 3 . withUnits groot 3 $ campaignFor StandardCampaign 1 [groot, rocket]
          bought = recordInLog (BuyMarketCard groot "16150") c
      canBuy rocket "16150" bought `shouldBe` False
      List.lookup "16150" (marketLeft bought) `shouldBe` Nothing
      List.lookup "16151" (marketLeft bought) `shouldBe` Just 1

    check "sells only The Market's cards, and only in this campaign" do
      let c = withUnits groot 7 (campaignAt StandardCampaign 1)
          redSkull = newCampaign StandardCampaign [groot]
      canBuy groot "16020" c `shouldBe` False
      canBuy groot "16150" redSkull `shouldBe` False
      length marketUnitCosts `shouldBe` 28

    check "adds the cards bought to the player's deck, not counted toward its size" do
      let c = recordInLog (BuyMarketCard groot "16150") (withUnits groot 1 (campaignAt StandardCampaign 1))
          deck = starter "groot"
      List.lookup "16150" (campaignDeck c deck).cards `shouldBe` Just 1
      campaignDeckShortBy c deck `shouldBe` 0

    check "keeps The Market's cards out of decks brought to the table" do
      marketCardsIn ((starter "groot") {cards = [("16150", 1), ("16002", 1)]}) `shouldBe` ["16150"]
      marketCardsIn (starter "groot") `shouldBe` []

  describe "Scenario #1: Brotherhood of Badoon" do
    it "brings the campaign's rules with its story and Setup instructions" do
      tg <- dealCampaignUnread (campaignAt ExpertCampaign 0) [starter "groot"]
      inGame tg do
        readStory
          `shouldReturn` map ("$story.galaxy." <>) ["badoon.intro", "campaignMode", "keywords", "market", "expertCampaign", "badoon.setup", "badoon.expertSetup"]

    it "reveals Badoon Blitz with its hinder threat and shuffles Badoon Headhunter into the encounter deck" do
      tg <- dealCampaign (campaignAt StandardCampaign 0) [starter "groot"]
      inGame tg do
        sideSchemeCodes >>= (liftIO . (`shouldContain` ["16178a"]))
        deck <- map toCardCode <$> scenarioEncounterDeckNow
        liftIO $ deck `shouldContain` ["16183"]
        liftIO $ filter (`elem` ["16184", "16185", "16186", "16187"]) deck `shouldBe` []

    it "reveals the expert side in an expert campaign, and each player puts the first minion they discard into play engaged with them" do
      tg <- dealCampaign (campaignAt ExpertCampaign 0) [starter "groot"]
      inGame tg do
        sideSchemeCodes >>= (liftIO . (`shouldContain` ["16178b"]))
        minions <- selectList (MinionEngagedWith (IdentityWithId (player1 tg)))
        liftIO $ length minions `shouldBe` 1

    it "won: 1 unit, 1 for each victory point to 3, 1 with no minions in play; Badoon Headhunter in the victory display marked" do
      tg <- dealCampaign (campaignAt StandardCampaign 0) [starter "groot"]
      inGame tg do
        toFirstTurn
        headhunter <- encounterCard =<< pure (fromMaybe (error "no headhunter") (lookup "16183" allEncounterCards))
        withGame_ $ victoryDisplayL .~ [headhunter]
        win
        readStory `shouldReturn` ["$story.galaxy.badoon.victory"]
        getsGame gameState `shouldReturn` Finished Won
        c <- campaignNow
        liftIO $ c.completed `shouldBe` 1
        liftIO $ c.campaignLog.galaxy.headhunterDefeated `shouldBe` [1]
        (.units) <$> grootNow `shouldReturn` 4

    it "records an expert campaign's remaining hit points with a win" do
      tg <- dealCampaign (campaignAt ExpertCampaign 0) [starter "groot"]
      inGame tg do
        toFirstTurn
        let p = player1 tg
        hp <- hitPointsOf p
        setDamage p 3
        win
        void readStory
        (.hitPoints) <$> (fromMaybe (error "no Groot") . campaignPlayer groot <$> campaignNow) `shouldReturn` Just (hp - 3)

  describe "Scenarios #2 to #5" do
    it "Infiltrate the Museum: Gallery of Splendor, and On the Hunt with a mark in Headhunter Defeated?" do
      let c = recordInLog (RecordHeadhunterDefeated 1) (campaignAt StandardCampaign 1)
      tg <- dealCampaign c [starter "groot"]
      inGame tg do
        sideSchemeCodes >>= (liftIO . (`shouldContain` ["16179a"]))
        deck <- map toCardCode <$> scenarioEncounterDeckNow
        liftIO $ filter (`elem` ["16183", "16184", "16185"]) deck `shouldMatchList` ["16183", "16184"]

    it "Nebula: the Headhunter's cards by the marks, as many as the scenario names" do
      let c = foldr (recordInLog . RecordHeadhunterDefeated) (campaignAt StandardCampaign 3) [1, 2, 3]
      tg <- dealCampaign c [starter "groot"]
      inGame tg do
        sideSchemeCodes >>= (liftIO . (`shouldContain` ["16181a"]))
        deck <- map toCardCode <$> scenarioEncounterDeckNow
        liftIO $ filter (`elem` ["16183", "16184", "16185", "16186", "16187"]) deck `shouldMatchList` ["16183", "16184", "16185", "16186"]

    it "Infiltrate the Museum won: the units added to those unspent" do
      tg <- dealCampaign (withUnits groot 2 (campaignAt StandardCampaign 1)) [starter "groot"]
      inGame tg do
        toFirstTurn
        placeMainThreat 0
        before <- mainSchemeThreatNow
        win
        void readStory
        -- 1, none for victory points, 1 for 1 per player or fewer in The Collection, and 1
        -- with no threat on the main scheme
        (.units) <$> grootNow `shouldReturn` (2 + 2 + if before == 0 then 1 else 0)

    it "expert: hit points as recorded, and a unit may be spent to heal to full" do
      let c = withUnits groot 1 (recordInLog (RecordHitPoints groot 5) (campaignAt ExpertCampaign 1))
      tg <- dealCampaign c [starter "groot"]
      inGame tg do
        choose "spend a unit" (isLabel "$label.spendUnitToHeal")
        toFirstTurn
        let p = player1 tg
        damageOf p `shouldReturn` 0
        (.units) <$> grootNow `shouldReturn` 0

    it "expert: keeping the hit points recorded keeps the unit" do
      let c = withUnits groot 1 (recordInLog (RecordHitPoints groot 5) (campaignAt ExpertCampaign 1))
      tg <- dealCampaign c [starter "groot"]
      inGame tg do
        choose "keep" (isLabel "$label.keepHitPoints")
        toFirstTurn
        hp <- hitPointsOf (player1 tg)
        damageOf (player1 tg) `shouldReturn` (hp - 5)
        (.units) <$> grootNow `shouldReturn` 1

    it "expert: no unit, no healing; a player defeated last scenario rejoins healed for nothing" do
      let hurt = recordInLog (RecordHitPoints groot 5) (campaignAt ExpertCampaign 1)
          defeated = recordInLog (RecordHitPoints groot 0) (campaignAt ExpertCampaign 1)
      tg <- dealCampaign hurt [starter "groot"]
      inGame tg do
        toFirstTurn
        hp <- hitPointsOf (player1 tg)
        damageOf (player1 tg) `shouldReturn` (hp - 5)
      tg' <- dealCampaign defeated [starter "groot"]
      inGame tg' do
        toFirstTurn
        damageOf (player1 tg') `shouldReturn` 0

    it "Escape the Museum: each player's cards recorded in The Collection are removed from the game" do
      let c = recordInLog (RecordCollection groot ["16002"]) (campaignAt StandardCampaign 2)
      tg <- dealCampaign c [starter "groot"]
      inGame tg do
        sideSchemeCodes >>= (liftIO . (`shouldContain` ["16180a"]))
        let p = player1 tg
        toFirstTurn
        deck <- getsGame $ \g -> maybe [] (\pl -> unDeck (toAttrs pl).playerIdentityDeck <> unHand (toAttrs pl).playerIdentityHand) (lookup p (gamePlayers g))
        liftIO $ length (filter ((== "16002") . toCardCode) deck) `shouldBe` 1

    it "Ronan: Kree Supremacy is the players' choice, and expert places 1 per player more threat on the main scheme" do
      let at mode = campaignAt mode 4
      tg <- dealCampaign (at StandardCampaign) [starter "groot"]
      standard <- inGame tg do
        choose "reveal Kree Supremacy" (isCard "16182a")
        sideSchemeCodes >>= (liftIO . (`shouldContain` ["16182a"]))
        mainSchemeThreatNow
      tg' <- dealCampaign (at ExpertCampaign) [starter "groot"]
      expert <- inGame tg' do
        choose "no Kree Supremacy" (isLabel "$label.pass")
        sideSchemeCodes >>= (liftIO . (`shouldNotContain` ["16182b"]))
        mainSchemeThreatNow
      expert `shouldBe` standard + 1

    it "Ronan won: the campaign is won; lost in an expert campaign, it is lost" do
      tg <- dealCampaign (campaignAt StandardCampaign 4) [starter "groot"]
      inGame tg do
        choose "no Kree Supremacy" (isLabel "$label.pass")
        toFirstTurn
        win
        readStory `shouldReturn` ["$story.galaxy.ronan.victory", "$story.galaxy.conclusion"]
        c <- campaignNow
        liftIO $ campaignWon c `shouldBe` True
      tg' <- dealCampaign (campaignAt ExpertCampaign 4) [starter "groot"]
      inGame tg' do
        choose "no Kree Supremacy" (isLabel "$label.pass")
        toFirstTurn
        withGame_ $ questionL .~ mempty
        pushAll [GameOver Lost]
        runGameMessages
        (.lost) <$> campaignNow `shouldReturn` True

  describe "the Challenge side schemes" do
    it "Badoon Blitz: hinder 3 per player, victory 1; defeated, each player may draw 1 card" do
      tg <- testGame ["groot"]
      let p = player1 tg
      inGame tg do
        card <- revealNow p SideSchemes.badoonBlitz
        let sid = sideSchemeOf card
        sideSchemeThreat sid `shouldReturn` 5
        before <- length <$> handOf p
        runNow [SideSchemeMessage sid (SideSchemeThwarted (IdentityRef p) 5)]
        choose "draw" (isLabelStarting "$label.drawCards")
        (length <$> handOf p) `shouldReturn` before + 1
        map toCardCode <$> getVictoryDisplay `shouldReturn` ["16178a"]

    it "Badoon Blitz may be declined; its expert side has each player discard a card" do
      tg <- testGame ["groot"]
      let p = player1 tg
      inGame tg do
        card <- revealNow p SideSchemes.badoonBlitzExpert
        let sid = sideSchemeOf card
        sideSchemeThreat sid `shouldReturn` 7
        before <- length <$> handOf p
        runNow [SideSchemeMessage sid (SideSchemeThwarted (IdentityRef p) 7)]
        hand <- handOf p
        choose "discard" (isTargetChoice (headMay hand `orElse` error "empty hand"))
        (length <$> handOf p) `shouldReturn` before - 1
      tg' <- testGame ["groot"]
      inGame tg' do
        card <- revealNow (player1 tg') SideSchemes.badoonBlitz
        before <- length <$> handOf (player1 tg')
        runNow [SideSchemeMessage (sideSchemeOf card) (SideSchemeThwarted (IdentityRef (player1 tg')) 5)]
        choose "pass" (isLabel "$label.pass")
        (length <$> handOf (player1 tg')) `shouldReturn` before

    it "\"There is No Escape\": defeated, 1 damage to each player, 2 on its expert side" do
      for_ [(SideSchemes.thereIsNoEscape, 7, 1), (SideSchemes.thereIsNoEscapeExpert, 9, 2)] \(def, threat, dmg) -> do
        tg <- testGame ["groot", "rocket-raccoon"]
        inGame tg do
          card <- revealNow (player1 tg) def
          let sid = sideSchemeOf card
          -- base threat and hinder per player, 2 players
          sideSchemeThreat sid `shouldReturn` (threat + if dmg == 1 then 3 else 4)
          threatNow <- sideSchemeThreat sid
          resolve [SideSchemeMessage sid (SideSchemeThwarted (IdentityRef (player1 tg)) threatNow)]
          damageOf (player1 tg) `shouldReturn` dmg
          damageOf (player2 tg) `shouldReturn` dmg

    it "Guerrilla Tactics and Kree Supremacy: their base threat and hinder, victory 1" do
      tg <- testGame ["groot"]
      let p = player1 tg
      inGame tg do
        for_ [(SideSchemes.guerrillaTactics, 8), (SideSchemes.guerrillaTacticsExpert, 10), (SideSchemes.kreeSupremacy, 9), (SideSchemes.kreeSupremacyExpert, 11)] \(def, threat) -> do
          card <- revealNow p def
          sideSchemeThreat (sideSchemeOf card) `shouldReturn` threat
        card <- revealNow p SideSchemes.galleryOfSplendor
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 6
        resolve [SideSchemeMessage (sideSchemeOf card) (SideSchemeThwarted (IdentityRef p) 6)]
        map toCardCode <$> getVictoryDisplay `shouldReturn` ["16179a"]
 where
  isCard code = \case
    CardLabel c _ -> c == code
    _ -> False
  orElse = flip fromMaybe
