-- | The Once and Future Kang: Kang's stages across separate game areas (TOAFK insert
-- pp.3-7), and the Kang set's cards that ask something new of the engine.
module Marvel.Encounter.KangSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.List (nub, sort)
import Marvel.Attack (Attack (attackDamage))
import Marvel.Area
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.MainScheme.MainSchemes.KangsWrath (kangsDominionDeck)
import Marvel.MainScheme.Types (getMainSchemeThreat)
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier (Modifier (CannotTakeDamage))
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Obligation.Types qualified as Obligation
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Villain.Types (villainIsTough)
import Marvel.Query
import Marvel.Phase (Phase (VillainPhase))
import Marvel.Scenario.Types (Attrs (scenarioSetAsideCards))
import TestImport

kang :: [Text] -> IO TestGame
kang = testGameAgainst "11001"

-- | The cards in play by code, in each area as the game lists them.
villainCodesIn :: AreaId -> GameAppT [CardCode]
villainCodesIn aid = do
  vs <- getVillainsInArea aid
  getsGame \g -> [toCardCode (getCardDef v) | vid <- vs, Just v <- [HashMap.lookup vid (gameVillains g)]]

schemeCodesIn :: AreaId -> GameAppT [CardCode]
schemeCodesIn aid = catMaybes <$> (traverse getMainSchemeCode =<< getMainSchemesIn aid)

villainCodes :: GameAppT [CardCode]
villainCodes = getsGame (sort . map (toCardCode . getCardDef) . HashMap.elems . gameVillains)

-- | The phase ends: what was to happen at its end happens.
endOfPhase :: GameAppT ()
endOfPhase = resolve [EndPhase VillainPhase]

-- | The villain of the player's area is defeated.
defeatKangOf :: IdentityId -> GameAppT ()
defeatKangOf p = do
  v <- asYou p (selectJust ActiveVillain)
  resolve [VillainMessage v VillainDefeated]

-- | Kang (I) defeated, and the phase over: stage 2, the players each in an area.
toStage2 :: TestGame -> GameAppT ()
toStage2 tg = defeatKangOf (player1 tg) >> endOfPhase

areaOfPlayer :: IdentityId -> GameAppT AreaId
areaOfPlayer p = areaId <$> getAreaOf p

-- | The area with The Master of Time.
central :: GameAppT AreaId
central = areaId . fromMaybe (error "no areas") . headMay <$> getAreas

stage3s, kang2s :: [CardCode]
stage3s = ["11009", "11010", "11011", "11012"]
kang2s = ["11002", "11003", "11004", "11005"]

spec :: Spec
spec = describe "The Once and Future Kang" do
  describe "setup" do
    it "has Kang (I) the only villain, the Dominions set aside, no player obligations, and deals each player a card" do
      tg <- kang ["spider-man", "she-hulk"]
      inGame tg do
        villainCodes `shouldReturn` ["11001"]
        codes <- map toCardCode <$> scenarioEncounterDeckNow
        liftIO $ "11023" `elem` codes `shouldBe` False
        setAside <- getsGame (map toCardCode . scenarioSetAsideCards . toAttrs . gameScenario)
        liftIO $ length (filter (== "11023") setAside) `shouldBe` 4
        -- neither She-Hulk's Legal Work nor Spider-Man's Eviction Notice
        liftIO $ any (`elem` ["01160", "01165"]) codes `shouldBe` False
        ps <- players
        dealt <- traverse (fmap length . dealtTo) ps
        liftIO $ all (>= 1) dealt `shouldBe` True

  describe "stage 2: separate game areas" do
    it "Kang (I) defeated, the main scheme advances at the end of the phase, not before" do
      tg <- kang ["spider-man"]
      inGame tg do
        defeatKangOf (player1 tg)
        villainCodes `shouldReturn` []
        (schemeCodesIn =<< central) `shouldReturn` ["11007"]
        length <$> getAreas `shouldReturn` 1
        endOfPhase
        length <$> getAreas `shouldReturn` 2

    it "splits two players into areas of their own, each with a stage 3 and its Kang (II); 2B stays in the first area" do
      tg <- kang ["spider-man", "she-hulk"]
      inGame tg do
        toStage2 tg
        [c, a1, a2] <- map areaId <$> getAreas
        [p1, p2] <- players
        schemeCodesIn c `shouldReturn` ["11008"]
        areaPlayers <$> getAreaOf p1 `shouldReturn` [p1]
        areaOfPlayer p1 `shouldReturn` a1
        areaOfPlayer p2 `shouldReturn` a2
        [s1] <- schemeCodesIn a1
        [s2] <- schemeCodesIn a2
        [k1] <- villainCodesIn a1
        [k2] <- villainCodesIn a2
        liftIO do
          (s1 /= s2, s1 `elem` stage3s, s2 `elem` stage3s) `shouldBe` (True, True, True)
          (k1 /= k2, k1 `elem` kang2s, k2 `elem` kang2s) `shouldBe` (True, True, True)
        -- each the villain of their own area, and no other
        v1 <- asYou p1 (selectList AnyEnemy)
        v2 <- asYou p2 (selectList AnyEnemy)
        liftIO $ any (`elem` v2) v1 `shouldBe` False

    it "splits three players into three areas, each with its own Kang (II)" do
      tg <- kang ["spider-man", "she-hulk", "iron-man"]
      inGame tg do
        toStage2 tg
        areas <- getAreas
        ps <- players
        liftIO $ length areas `shouldBe` 4
        own <- traverse areaOfPlayer ps
        kangs <- traverse villainCodesIn own
        liftIO do
          length (nub own) `shouldBe` 3
          all ((== 1) . length) kangs `shouldBe` True
          length (nub (concat kangs)) `shouldBe` 3

    it "turns each side scheme in play into an acceleration token, which adds threat to no stage 3" do
      tg <- kang ["spider-man", "she-hulk"]
      inGame tg do
        _ <- revealNow (player1 tg) SideSchemes.rampage
        tokens <- accelerationTokensNow
        toStage2 tg
        accelerationTokensNow `shouldReturn` tokens + 1
        getsGame (length . gameSideSchemes) `shouldReturn` 0
        [s1] <- getMainSchemesIn =<< areaOfPlayer (player1 tg)
        resolve [PlaceAccelerationToken s1]
        before <- stageThreats
        resolve [AccelerateMainScheme]
        after <- stageThreats
        -- each stage 3 its own escalation threat, 1, and no token's
        liftIO $ zipWith (-) after before `shouldBe` [1, 1]

    it "The Master of Time takes an acceleration token placed on a stage 3, and its tokens add threat to no stage 3" do
      tg <- kang ["spider-man", "she-hulk"]
      inGame tg do
        toStage2 tg
        here <- central
        [s1] <- getMainSchemesIn =<< areaOfPlayer (player1 tg)
        tokens <- getAccelerationCountIn here
        resolve [PlaceAccelerationToken s1]
        getAccelerationCountIn here `shouldReturn` tokens + 1
        a1 <- areaOfPlayer (player1 tg)
        getsGame (map areaAccelerationTokens . filter ((== a1) . areaId) . gameAreas) `shouldReturn` [0]

  describe "joining another game area" do
    it "a player whose Kang (II) is defeated joins another area at the end of the phase, not The Master of Time's while another remains; side schemes and minions come along" do
      tg <- kang ["spider-man", "she-hulk", "iron-man"]
      inGame tg do
        toStage2 tg
        [p1, p2, p3] <- players
        [a1, a2, a3] <- traverse areaOfPlayer [p1, p2, p3]
        side <- sideSchemeOf <$> revealNow p1 SideSchemes.rampage
        minion <- addMinion p1 Minions.macrobots
        defeatKangOf p1
        -- his stage 3 is removed from the game at once
        schemeCodesIn a1 `shouldReturn` []
        start [EndPhase VillainPhase]
        [m2] <- getMainSchemesIn a2
        [m3] <- getMainSchemesIn a3
        Just (who, cs) <- asked
        liftIO do
          who `shouldBe` p1
          sort (map tshow [t | TargetLabel t _ <- cs]) `shouldBe` sort (map tshow [MainSchemeRef m2, MainSchemeRef m3])
        choose "join the second player" (isTargetChoice (MainSchemeRef m2))
        areaOfPlayer p1 `shouldReturn` a2
        getAreaOfTarget (SideSchemeRef side) `shouldReturn` a2
        getAreaOfTarget (MinionRef minion) `shouldReturn` a2
        length <$> getAreas `shouldReturn` 3

    it "copies of a unique card are discarded down to one as areas combine, the first player choosing" do
      tg <- kang ["spider-man", "she-hulk"]
      inGame tg do
        toStage2 tg
        [p1, p2] <- players
        _ <- putIntoPlay p1 "01084"
        -- a unique card in one area places no limit on another
        asYou p2 getUniqueTitlesInPlay >>= (`shouldSatisfy` notElem ("Nick Fury", []))
        _ <- putIntoPlay p2 "01084"
        defeatKangOf p1
        start [EndPhase VillainPhase]
        Just (who, _) <- asked
        liftIO $ who `shouldBe` p1
        [kept] <- filter ((== p2) . allyController) <$> alliesWith "01084"
        choose "keep the second player's" (isTargetChoice (AllyRef $ allyId kept))
        map allyId <$> alliesWith "01084" `shouldReturn` [allyId kept]

    it "a stage 3 completed puts a Kang's Dominion under stage 4A; at the end of the phase its Kang and it leave the game, and the last area goes to The Master of Time, which advances" do
      tg <- kang ["spider-man"]
      let p = player1 tg
      inGame tg do
        toStage2 tg
        tokens <- accelerationTokensNow
        a <- areaOfPlayer p
        [stage] <- getMainSchemesIn a
        resolve [MainSchemeMessage stage (MainSchemePlaceThreat 9)]
        length . pileDeck <$> getPile kangsDominionDeck `shouldReturn` 1
        -- again completed, no second Dominion
        resolve [MainSchemeMessage stage (MainSchemePlaceThreat 1)]
        length . pileDeck <$> getPile kangsDominionDeck `shouldReturn` 1
        endOfPhase
        length <$> getAreas `shouldReturn` 1
        villainCodes `shouldReturn` ["11006"]
        (schemeCodesIn =<< central) `shouldReturn` ["11013"]
        dominions <- getsGame (filter ((== "11023") . toCardCode . getCardDef) . HashMap.elems . gameSideSchemes)
        liftIO $ length dominions `shouldBe` 1
        -- 4B: Spider-Man's nemesis minion, engaged with him
        vultures <- getsGame (filter ((== "01167") . toCardCode . getCardDef) . HashMap.elems . gameMinions)
        liftIO $ length vultures `shouldBe` 1
        -- the tokens kept at The Master of Time add threat to stage 4
        before <- mainSchemeThreatNow
        resolve [AccelerateMainScheme]
        mainSchemeThreatNow `shouldReturn` before + 1 + tokens

    it "a stage 3 is completed when all the players at it are defeated" do
      tg <- kang ["spider-man", "she-hulk"]
      inGame tg do
        toStage2 tg
        [p1, p2] <- players
        resolve [RemoveFromPlay (IdentityRef p2)]
        length . pileDeck <$> getPile kangsDominionDeck `shouldReturn` 1
        endOfPhase
        length <$> getAreas `shouldReturn` 2
        a1 <- areaOfPlayer p1
        villainCodesIn a1 >>= (`shouldSatisfy` ((== 1) . length))

  describe "cards" do
    it "Kang (I): when he attacks you, place 1 threat on the main scheme or he gets +2 ATK" do
      tg <- kang ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        k <- villainIdNow
        start [VillainMessage k (VillainAttacks p)]
        chooseIf isNoResponse
        chooseIf isAnyAbility
        offers (isLabel "$label.kangPlacesThreat") `shouldReturn` True
        choose "+2 ATK" (isLabel "$label.kangGetsAttack")
        fmap attackDamage <$> getVillainAttack k `shouldReturn` Just 4

    it "Kang (Immortus) cannot take damage while a minion is in play in his area" do
      tg <- kang ["spider-man"]
      let p = player1 tg
      inGame tg do
        before <- getsGame (HashMap.keys . gameVillains)
        resolve [AddVillain "11002"]
        [immortus] <- getsGame (filter (`notElem` before) . HashMap.keys . gameVillains)
        getModifiers immortus >>= (`shouldSatisfy` notElem CannotTakeDamage)
        _ <- addMinion p Minions.macrobots
        getModifiers immortus >>= (`shouldSatisfy` elem CannotTakeDamage)

    it "Kang's Dominion: Kang cannot take damage; its defeater is dealt an encounter card" do
      tg <- kang ["spider-man"]
      let p = player1 tg
      inGame tg do
        k <- villainIdNow
        dominion <- sideSchemeOf <$> revealNow p SideSchemes.kangsDominion
        getModifiers k >>= (`shouldSatisfy` elem CannotTakeDamage)
        dealt <- length <$> dealtTo p
        threat <- sideSchemeThreat dominion
        resolve [SideSchemeMessage dominion (SideSchemeThwarted (IdentityRef p) threat)]
        length <$> dealtTo p `shouldReturn` dealt + 1
        getModifiers k >>= (`shouldSatisfy` notElem CannotTakeDamage)

    it "Temporal Shield prevents an attack's damage and deals 1 to the attacker" do
      tg <- kang ["spider-man"]
      let p = player1 tg
      inGame tg do
        k <- villainIdNow
        _ <- revealNow p Attachments.temporalShield
        changeForm p
        before <- damageOf p
        choose "attack" (isHeroAttack p)
        choose "the target" (isTargetChoice (VillainRef k))
        chooseIf isAnyAbility
        pass
        damageOf p `shouldReturn` before + 1
        villainIsTough <$> getVillain `shouldReturn` True
        getsGame (length . gameAttachments) `shouldReturn` 0

    it "Stolen Memories holds the top 8 cards of your deck; discarded, so are they" do
      tg <- kang ["spider-man"]
      let p = player1 tg
      inGame tg do
        deck <- unDeck . (.playerIdentityDeck) <$> playerAttrs p
        _ <- revealNow p Obligations.stolenMemories
        length . unDeck . (.playerIdentityDeck) <$> playerAttrs p `shouldReturn` length deck - 8
        [o] <- getsGame (HashMap.toList . gameObligations)
        liftIO $ map pcCardId (Obligation.obligationHeldCards (toAttrs (snd o))) `shouldBe` map pcCardId (take 8 deck)
        clearHand p
        mental <- putInHand p "01089"
        choose "discard Stolen Memories" (isAbilityOf (ObligationRef (fst o)))
        choose "discard the mental resource" (isTargetChoice (CardIdRef $ pcCardId mental))
        obligationsInPlay `shouldReturn` 0
        discards <- map pcCardId <$> discardOf p
        liftIO $ all (`elem` discards) (map pcCardId (take 8 deck)) `shouldBe` True

    it "Time-Travel Hijinks discards the highest-cost card you control and places it under itself" do
      tg <- kang ["spider-man"]
      let p = player1 tg
      inGame tg do
        fury <- putIntoPlay p "01084"
        _ <- revealNow p Obligations.timeTravelHijinks
        pass
        alliesWith "01084" `shouldReturn` []
        [o] <- getsGame (HashMap.elems . gameObligations)
        liftIO $ map pcCardId (Obligation.obligationHeldCards (toAttrs o)) `shouldBe` [pcCardId fury]
        map pcCardId <$> discardOf p >>= (`shouldSatisfy` notElem (pcCardId fury))

    it "Depowered: you cannot play hero-specific cards" do
      tg <- kang ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        shooter <- putInHand p "01008"
        _ <- putInHand p "01089"
        offers (isPlay shooter) `shouldReturn` True
        _ <- revealNow p Obligations.depowered
        offers (isPlay shooter) `shouldReturn` False

    it "Corrupted Timestream: no Alter-Ego Action of an obligation can be triggered" do
      tg <- kang ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putInHand p "01089"
        _ <- revealNow p Obligations.stolenMemories
        [o] <- getsGame (HashMap.keys . gameObligations)
        offers (isAbilityOf (ObligationRef o)) `shouldReturn` True
        _ <- revealNow p SideSchemes.corruptedTimestream
        pass
        offers (isAbilityOf (ObligationRef o)) `shouldReturn` False

  describe "hazards" do
    it "a hazard icon deals more encounter cards to its own area's players only" do
      tg <- kang ["spider-man", "she-hulk"]
      inGame tg do
        toStage2 tg
        [p1, p2] <- players
        _ <- revealNow p1 SideSchemes.timePortal
        a1 <- areaOfPlayer p1
        a2 <- areaOfPlayer p2
        getHazardCountIn a1 `shouldReturn` 1
        getHazardCountIn a2 `shouldReturn` 0
 where
  stageThreats = do
    areas <- getAreas
    ms <- concat <$> traverse (getMainSchemesIn . areaId) (drop 1 areas)
    getsGame \g -> [getMainSchemeThreat m | mid <- ms, Just m <- [HashMap.lookup mid (gameMainSchemes g)]]
