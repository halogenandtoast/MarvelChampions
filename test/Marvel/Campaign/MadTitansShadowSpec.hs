-- | The Mad Titan's Shadow campaign: its log, and each scenario's Campaign Instructions
-- followed against the box's own scenarios ("docs/rules/campaign-mad-titans-shadow.md").
module Marvel.Campaign.MadTitansShadowSpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import Data.Functor.Identity (Identity (..))
import Data.HashMap.Strict qualified as HashMap
import Marvel.Campaign
import Marvel.EncounterCard (allEncounterCards)
import Marvel.Environment.Types (Environment, Attrs (..))
import TestImport

spiderMan, captainMarvel :: CardCode
spiderMan = "01001a"
captainMarvel = "01010a"

-- | The campaign with so many scenarios won, for the identities.
titanAt :: CampaignMode -> Int -> [CardCode] -> Campaign
titanAt mode n heroes = (newCampaignOf MadTitansShadow mode heroes) {completed = n}

mts :: CampaignMode -> Int -> Campaign
mts mode n = titanAt mode n [spiderMan]

withMarks :: [TitanMark] -> Campaign -> Campaign
withMarks ms c = foldr (recordInLog . RecordTitanMark) c ms

-- | The campaign's next game against the box's own scenario at that place, nothing read.
dealUnread :: Campaign -> [DeckList] -> IO TestGame
dealUnread c decks = do
  let code = fromMaybe (error "campaign over") (currentScenario c)
      setup = fromMaybe (error "no such scenario") (recommendedSetup code)
  (app, ran, logged) <- dealtGameWith (campaignL ?~ c) setup decks 1
  seats <- runGameApp app (getsGame gameSeats)
  pure $ TestGame app ran logged seats

-- | Dealt, its story read.
deal :: Campaign -> [DeckList] -> IO TestGame
deal c decks = do
  tg <- dealUnread c decks
  tg <$ inGame tg readStory

readStory :: GameAppT [Text]
readStory =
  openQuestion >>= \case
    Just (ident, Read sections _) -> sections <$ (answerQuestion ident 0 >> pressMainSchemeSetup)
    q -> error $ "nothing to read: " <> show q

-- | Answer each question with its first choice until one offers a matching choice.
upTo :: (HasCallStack) => (Choice -> Bool) -> GameAppT ()
upTo p = go (200 :: Int)
 where
  go 0 = error "never offered"
  go n =
    asked >>= \case
      Just (ident, cs)
        | any p cs -> pure ()
        | otherwise -> answerQuestion ident 0 >> go (n - 1)
      Nothing -> error "nothing asked"

win :: GameAppT ()
win = do
  withGame_ $ questionL .~ mempty
  pushAll [GameOver Won]
  runGameMessages

lose :: GameAppT ()
lose = do
  withGame_ $ questionL .~ mempty
  pushAll [GameOver Lost]
  runGameMessages

campaignNow :: GameAppT Campaign
campaignNow = getsGame (fromMaybe (error "no campaign") . gameCampaign)

marksNow :: GameAppT [TitanMark]
marksNow = (.campaignLog.titan.marks) <$> campaignNow

sideSchemeCodes :: GameAppT [CardCode]
sideSchemeCodes = map fst <$> getSideSchemesInPlay

sideSchemeIn :: CardCode -> GameAppT (Maybe SideSchemeId)
sideSchemeIn code = getsGame (headMay . map fst . filter ((== code) . toCardCode . getCardDef . snd) . HashMap.toList . gameSideSchemes)

encounterDef :: CardCode -> CardDef
encounterDef code = fromMaybe (error "no such card") (lookup code allEncounterCards)

deckCodes :: IdentityId -> GameAppT [CardCode]
deckCodes p = map toCardCode . unDeck . (.playerIdentityDeck) <$> playerAttrs p

isCard :: CardCode -> Choice -> Bool
isCard code = \case
  CardLabel c _ -> c == code
  _ -> False

spec :: Spec
spec = describe "The Mad Titan's Shadow campaign" do
  describe "the log" do
    it "plays its five scenarios, its section empty" $ (id :: IO () -> IO ()) do
      let c = newCampaignOf MadTitansShadow StandardCampaign [spiderMan]
      c.scenarios `shouldBe` madTitansShadow
      c.campaignLog.titan.marks `shouldBe` []

    it "checks a box once, and loads a log saved before it had its section" $ (id :: IO () -> IO ()) do
      let c = withMarks [CosmoInPool, CosmoInPool, OdinInPool] (mts StandardCampaign 0)
      c.campaignLog.titan.marks `shouldBe` [OdinInPool, CosmoInPool]
      hasMark CosmoInPool c `shouldBe` True
      hasMark ShawarmaInPool c `shouldBe` False
      fromJSON (toJSON c) `shouldBe` Success c
      let saved = case toJSON (newCampaign StandardCampaign [spiderMan]) of
            Object o -> Object (runIdentity (KeyMap.alterF (Identity . fmap dropTitan) "campaignLog" o))
            v -> v
          dropTitan = \case
            Object l -> Object (KeyMap.delete "titan" l)
            v -> v
      case fromJSON saved of
        Success (old :: Campaign) -> old.campaignLog.titan.marks `shouldBe` []
        Error e -> expectationFailure e

    it "keeps its campaign-only cards out of decks brought to the table" $ (id :: IO () -> IO ()) do
      campaignOnlyCardsIn ((starter "spider-man") {cards = [("21183", 1), ("01088", 1)]}) `shouldBe` ["21183"]
      campaignOnlyCardsIn ((starter "spider-man") {cards = [("21185", 1), ("21190", 1)]}) `shouldMatchList` ["21185", "21190"]
      campaignOnlyCardsIn (starter "spider-man") `shouldBe` []

  describe "Scenario #1: Ebony Maw" do
    it "brings the campaign's rules with its story and Setup instructions" do
      tg <- dealUnread (mts ExpertCampaign 0) [starter "spider-man"]
      inGame tg do
        readStory
          `shouldReturn` map ("$story.mts." <>) ["ebonyMaw.intro", "campaignMode", "expertCampaign", "ebonyMaw.rules", "ebonyMaw.setup"]

    it "puts Secure the Landing Pad into play and shuffles Security Breach into the encounter deck" do
      tg <- deal (mts StandardCampaign 0) [starter "spider-man"]
      inGame tg do
        toFirstTurn
        sideSchemeCodes >>= (liftIO . (`shouldContain` ["21180a"]))
        deck <- map toCardCode <$> scenarioEncounterDeckNow
        liftIO $ deck `shouldContain` ["21181"]

    it "won with Secure the Landing Pad defeated and Attack on Knowhere 1B completed: Cosmo and Security Breach to the pool" do
      tg <- deal (mts StandardCampaign 0) [starter "spider-man"]
      inGame tg do
        toFirstTurn
        let p = player1 tg
        Just pad <- sideSchemeIn "21180a"
        threat <- sideSchemeThreat pad
        resolve [SideSchemeMessage pad (SideSchemeThwarted (IdentityRef p) threat)]
        m <- mainSchemeIdNow
        resolve [AdvanceMainScheme m]
        win
        readStory `shouldReturn` ["$story.mts.ebonyMaw.victory"]
        marksNow >>= (liftIO . (`shouldMatchList` [CosmoInPool, SecurityBreachInPool]))
        (.completed) <$> campaignNow `shouldReturn` 1

    it "won with neither: nothing to the pool" do
      tg <- deal (mts StandardCampaign 0) [starter "spider-man"]
      inGame tg do
        toFirstTurn
        win
        void readStory
        marksNow `shouldReturn` []

    it "expert: the remaining hit points recorded, no more than printed" do
      tg <- deal (mts ExpertCampaign 0) [starter "spider-man"]
      inGame tg do
        toFirstTurn
        let p = player1 tg
        hp <- hitPointsOf p
        setDamage p 3
        win
        readStory `shouldReturn` ["$story.mts.ebonyMaw.victory", "$story.mts.ebonyMaw.expertVictory"]
        (.hitPoints) <$> (fromMaybe (error "no Spider-Man") . campaignPlayer spiderMan <$> campaignNow) `shouldReturn` Just (hp - 3)

    it "expert, two players, one eliminated: the survivor's remaining hit points recorded, the eliminated player's 0; the survivor reads the victory" do
      tg <- deal (titanAt ExpertCampaign 0 [spiderMan, captainMarvel]) [starter "spider-man", starter "captain-marvel"]
      let (p1, p2) = (player1 tg, player2 tg)
      inGame tg do
        toFirstTurn
        hp <- hitPointsOf p1
        setDamage p1 3
        resolve [IdentityMessage p2 IdentityDefeated]
        win
        openQuestion >>= \case
          Just (ident, Read _ _) -> liftIO $ ident `shouldBe` p1
          q -> liftIO $ expectationFailure ("no victory to read: " <> show q)
        void readStory
        c <- campaignNow
        liftIO do
          (.hitPoints) <$> campaignPlayer spiderMan c `shouldBe` Just (Just (hp - 3))
          (.hitPoints) <$> campaignPlayer captainMarvel c `shouldBe` Just (Just 0)
          c.completed `shouldBe` 1

  describe "Scenario #2: Tower Defense" do
    it "puts Save the Shawarma Place into play, and Security Breach in the encounter deck only from the pool" do
      tg <- deal (mts StandardCampaign 1) [starter "spider-man"]
      inGame tg do
        toFirstTurn
        sideSchemeCodes >>= (liftIO . (`shouldContain` ["21182a"]))
        deck <- map toCardCode <$> scenarioEncounterDeckNow
        liftIO $ deck `shouldNotContain` ["21181"]
      tg' <- deal (withMarks [SecurityBreachInPool] (mts StandardCampaign 1)) [starter "spider-man"]
      inGame tg' do
        toFirstTurn
        deck <- map toCardCode <$> scenarioEncounterDeckNow
        liftIO $ deck `shouldContain` ["21181"]

    it "won: Shawarma with the scheme defeated, Black Swan not in the victory display, Avengers Tower Damaged" do
      tg <- deal (mts StandardCampaign 1) [starter "spider-man"]
      inGame tg do
        toFirstTurn
        let p = player1 tg
        Just place <- sideSchemeIn "21182a"
        threat <- sideSchemeThreat place
        resolve [SideSchemeMessage place (SideSchemeThwarted (IdentityRef p) threat)]
        -- Avengers Tower's Stronghold side flipped by 9 per player damage
        tower <- getsGame (fromMaybe (error "no Avengers Tower") . headMay . map toAttrs . HashMap.elems . gameEnvironments)
        resolve [EnvironmentMessage (environmentId tower) (EnvironmentDamaged 9)]
        win
        void readStory
        marksNow >>= (liftIO . (`shouldMatchList` [ShawarmaInPool, BlackSwanInPool, AvengersTowerDamaged]))

    it "won with Black Swan in the victory display: she is not added" do
      tg <- deal (mts StandardCampaign 1) [starter "spider-man"]
      inGame tg do
        toFirstTurn
        swan <- encounterCard (encounterDef "21182b")
        withGame_ $ victoryDisplayL .~ [swan]
        win
        void readStory
        marksNow `shouldReturn` []

    it "expert: hit points as recorded; a player may place an acceleration token on either main scheme to heal" do
      tg <- deal (recordInLog (RecordHitPoints spiderMan 5) (mts ExpertCampaign 1)) [starter "spider-man"]
      inGame tg do
        upTo (isLabel "$label.keepHitPoints")
        schemes <- getsGame (HashMap.keys . gameMainSchemes)
        for_ schemes \m -> offers (isTargetChoice m) `shouldReturn` True
        liftIO $ length schemes `shouldBe` 2
        tokens <- accelerationTokensNow
        choose "heal" (isTargetChoice (headMay schemes `orElse` error "no main scheme"))
        toFirstTurn
        damageOf (player1 tg) `shouldReturn` 0
        accelerationTokensNow `shouldReturn` tokens + 1

    it "expert: a player recorded at 0 places the token on the main scheme of their choice, and is healed" do
      tg <- deal (recordInLog (RecordHitPoints spiderMan 0) (mts ExpertCampaign 1)) [starter "spider-man"]
      inGame tg do
        schemes <- getsGame (HashMap.keys . gameMainSchemes)
        upTo (\c -> any (`isTargetChoice` c) schemes)
        offers (isLabel "$label.keepHitPoints") `shouldReturn` False
        for_ schemes \m -> offers (isTargetChoice m) `shouldReturn` True
        tokens <- accelerationTokensNow
        choose "heal" (isTargetChoice (lastMay schemes `orElse` error "no main scheme"))
        toFirstTurn
        damageOf (player1 tg) `shouldReturn` 0
        accelerationTokensNow `shouldReturn` tokens + 1

    it "expert: keeping the hit points recorded places no token" do
      tg <- deal (recordInLog (RecordHitPoints spiderMan 5) (mts ExpertCampaign 1)) [starter "spider-man"]
      inGame tg do
        upTo (isLabel "$label.keepHitPoints")
        tokens <- accelerationTokensNow
        choose "keep" (isLabel "$label.keepHitPoints")
        toFirstTurn
        hp <- hitPointsOf (player1 tg)
        damageOf (player1 tg) `shouldReturn` hp - 5
        accelerationTokensNow `shouldReturn` tokens

  describe "Scenario #3: Thanos" do
    it "Hack Sanctuary's Computer; Cosmo under the first player, Black Swan engaged with them, Shawarma in each deck, from the pool" do
      let c = withMarks [CosmoInPool, ShawarmaInPool, BlackSwanInPool, SecurityBreachInPool] (titanAt StandardCampaign 2 [spiderMan, captainMarvel])
      tg <- deal c [starter "spider-man", starter "captain-marvel"]
      let (p1, p2) = (player1 tg, player2 tg)
      inGame tg do
        -- the first player chosen before the scenario's setup, and its Campaign Setup
        choose "the second seat goes first" (isTargetChoice p2)
        toFirstTurn
        sideSchemeCodes >>= (liftIO . (`shouldContain` ["21184a"]))
        cosmo <- allyIn "21180b"
        swans <- getsGame (filter ((== "21182b") . toCardCode . minionCardDef) . map toAttrs . HashMap.elems . gameMinions)
        liftIO do
          allyController cosmo `shouldBe` p2
          map minionEngagedIdentity swans `shouldBe` [p2]
        encounter <- map toCardCode <$> scenarioEncounterDeckNow
        liftIO $ encounter `shouldContain` ["21181"]
        for_ [p1, p2] \p -> do
          owned <- (<>) <$> deckCodes p <*> (map toCardCode <$> handOf p)
          liftIO $ owned `shouldContain` ["21183"]

    it "Avengers Tower Damaged: three damage to each identity, after the expert healing" do
      tg <- deal (withMarks [AvengersTowerDamaged] (mts StandardCampaign 2)) [starter "spider-man"]
      inGame tg do
        toFirstTurn
        damageOf (player1 tg) `shouldReturn` 3
      tg' <- deal (withMarks [AvengersTowerDamaged] (recordInLog (RecordHitPoints spiderMan 5) (mts ExpertCampaign 2))) [starter "spider-man"]
      inGame tg' do
        upTo (isLabel "$label.keepHitPoints")
        mainScheme <- mainSchemeIdNow
        choose "heal" (isTargetChoice mainScheme)
        toFirstTurn
        damageOf (player1 tg') `shouldReturn` 3

    it "expert: a player defeated as the last scenario was won places the token and is healed, unasked" do
      tg <- deal (recordInLog (RecordHitPoints spiderMan 0) (mts ExpertCampaign 2)) [starter "spider-man"]
      inGame tg do
        offers (isLabel "$label.keepHitPoints") `shouldReturn` False
        toFirstTurn
        damageOf (player1 tg) `shouldReturn` 0
        accelerationTokensNow `shouldReturn` 1

    it "won: System Shock unless Defensive Protocols is in the victory display; The Infinity Stones 1B recorded if completed" do
      tg <- deal (mts StandardCampaign 2) [starter "spider-man"]
      inGame tg do
        toFirstTurn
        m <- mainSchemeIdNow
        resolve [AdvanceMainScheme m]
        win
        void readStory
        marksNow >>= (liftIO . (`shouldMatchList` [SystemShockInPool, InfinityStones1BCompleted]))
      tg' <- deal (mts StandardCampaign 2) [starter "spider-man"]
      inGame tg' do
        toFirstTurn
        protocols <- encounterCard (encounterDef "21184b")
        withGame_ $ victoryDisplayL .~ [protocols]
        win
        void readStory
        marksNow `shouldReturn` []

  describe "Scenario #4: Hela" do
    it "Find the Norn Stones; Summoned Back in the encounter deck; Shawarma and System Shock in each deck; the top half of each deck discarded" do
      tg <- deal (withMarks [ShawarmaInPool, SystemShockInPool, InfinityStones1BCompleted] (mts StandardCampaign 3)) [starter "spider-man"]
      let p = player1 tg
      inGame tg do
        toFirstTurn
        sideSchemeCodes >>= (liftIO . (`shouldContain` ["21186a"]))
        encounter <- map toCardCode <$> scenarioEncounterDeckNow
        deck <- deckCodes p
        hand <- map toCardCode <$> handOf p
        discard <- map toCardCode <$> discardOf p
        let owned = deck <> hand <> discard
        liftIO do
          encounter `shouldContain` ["21188"]
          owned `shouldContain` ["21183"]
          owned `shouldContain` ["21185"]
          length discard `shouldBe` length owned `div` 2

    it "won: Norn Stone with Find the Norn Stones defeated, Odin with Retrieve Odin's Armor in the victory display" do
      tg <- deal (mts StandardCampaign 3) [starter "spider-man"]
      inGame tg do
        toFirstTurn
        Just stones <- sideSchemeIn "21186a"
        resolve [RemoveFromGame (SideSchemeRef stones)]
        armor <- encounterCard (encounterDef "21186b")
        withGame_ $ victoryDisplayL .~ [armor]
        win
        void readStory
        marksNow >>= (liftIO . (`shouldMatchList` [NornStoneInPool, OdinInPool]))
      tg' <- deal (mts StandardCampaign 3) [starter "spider-man"]
      inGame tg' do
        toFirstTurn
        win
        void readStory
        marksNow `shouldReturn` []

  describe "Scenario #5: Loki" do
    it "the Captive allies set aside, Open the Dungeons, Summoned Back; Norn Stone on its Setup side and Odin on his King side from the pool" do
      tg <- deal (withMarks [NornStoneInPool, OdinInPool] (mts StandardCampaign 4)) [starter "spider-man"]
      let p = player1 tg
      inGame tg do
        toFirstTurn
        sideSchemeCodes >>= (liftIO . (`shouldContain` ["21189a"]))
        aside <- map toCardCode <$> getSetAsidePlayerCards
        liftIO $ aside `shouldMatchList` ["21190", "21191", "21192", "21193"]
        encounter <- map toCardCode <$> scenarioEncounterDeckNow
        liftIO $ encounter `shouldContain` ["21188"]
        stones <- upgradesWith "21187a"
        liftIO $ map upgradeController stones `shouldBe` [p]
        odin <- allyIn "21139b"
        liftIO $ allyController odin `shouldBe` p

    it "Open the Dungeons defeated: a Captive ally of the set chosen into play" do
      tg <- deal (mts StandardCampaign 4) [starter "spider-man"]
      let p = player1 tg
      inGame tg do
        toFirstTurn
        Just dungeons <- sideSchemeIn "21189a"
        threat <- sideSchemeThreat dungeons
        runNow [SideSchemeMessage dungeons (SideSchemeThwarted (IdentityRef p) threat)]
        choose "Volstagg" (isCard "21193")
        volstagg <- allyIn "21193"
        liftIO $ allyController volstagg `shouldBe` p

    it "won: the campaign is won; lost in an expert campaign, it is lost, in a standard one it is not" do
      tg <- deal (mts StandardCampaign 4) [starter "spider-man"]
      inGame tg do
        toFirstTurn
        win
        readStory `shouldReturn` ["$story.mts.loki.victory", "$story.mts.conclusion"]
        c <- campaignNow
        liftIO $ campaignWon c `shouldBe` True
      tg' <- deal (mts ExpertCampaign 4) [starter "spider-man"]
      inGame tg' do
        toFirstTurn
        lose
        (.lost) <$> campaignNow `shouldReturn` True
      tg'' <- deal (mts StandardCampaign 4) [starter "spider-man"]
      inGame tg'' do
        toFirstTurn
        lose
        (.lost) <$> campaignNow `shouldReturn` False

    it "brings its expert Setup instructions with its story" do
      tg <- dealUnread (mts ExpertCampaign 4) [starter "spider-man"]
      inGame tg do
        readStory `shouldReturn` map ("$story.mts." <>) ["loki.intro", "loki.rules", "loki.setup", "loki.expertSetup"]
 where
  orElse = flip fromMaybe
