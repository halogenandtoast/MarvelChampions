module Marvel.CivilWar.TeamSpec (spec) where

import Control.Exception (evaluate)
import Control.Monad.Random (evalRand)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict qualified as HashMap
import Marvel.CivilWar.Types qualified as CW
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers qualified as CWHelp
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as M
import Marvel.MainScheme.Types qualified as Main
import Marvel.Keyword qualified as K
import Marvel.EncounterSet qualified as E
import Marvel.Hp (unHp)
import Marvel.SideScheme.Cards qualified as S
import Marvel.Game.Public (publicGameFor)
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.Scenario.Types
import Marvel.Villain.Types qualified as V
import System.Random (mkStdGen)
import Test.Hspec qualified as H
import TestImport

newtype CompetitiveFixture = CompetitiveFixture (Attrs Scenario `With` CW.CompetitionSetup)
  deriving newtype (Show, Eq, ToJSON, FromJSON)
instance IsScenario CompetitiveFixture where
  toScenarioAttrs (CompetitiveFixture (attrs `With` _)) = attrs
  overScenarioAttrs f (CompetitiveFixture (attrs `With` setup)) = CompetitiveFixture (f attrs `With` setup)
  scenarioCompetitionSetup (CompetitiveFixture (_ `With` setup)) = Just setup
instance RunMessage CompetitiveFixture where
  runMessage msg (CompetitiveFixture (attrs `With` setup)) =
    (\attrs' -> CompetitiveFixture (attrs' `With` setup)) <$> runMessage msg attrs

setup :: CW.CompetitionSetup
setup = CW.CompetitionSetup
  (fromMaybe (error "missing Iron Man setup") $ CW.defaultTeamScenario "56059")
  (fromMaybe (error "missing Captain America setup") $ CW.defaultTeamScenario "56137")

competitive :: [Text] -> IO TestGame
competitive heroes = do
  let standard = ScenarioSetup "01094" [] Normal
      scenario = fromMaybe (error "missing Rhino") $ lookupScenario standard
      fixture = Scenario $ CompetitiveFixture (toAttrs scenario `With` setup)
  identities <- either (error . show) pure . sequence $ evalRand (traverse (loadPlayer . starter) heroes) (mkStdGen 1)
  seated <- case identities of
    [] -> error "missing fixture players"
    p : ps -> pure (p :| ps)
  app <- newGameApp (newTable fixture seated) [StartGame] (mkStdGen 1) (const $ pure ())
  ran <- newIORef []
  logged <- newIORef []
  let tg = TestGame app ran logged (map toId identities)
  inGame tg $ runGame >> toFirstTurn
  pure tg

addSecondBoard :: TestGame -> GameAppT VillainId
addSecondBoard tg = do
  let p = player1 tg
      q = last (seats tg)
  resolve [SetActivePlayer q, AddVillain "01113", AddMainScheme "01116", SetActivePlayer p]
  fromMaybe (error "missing second leader") <$> getActiveVillainIn (CW.teamArea CW.Resistance)

spec :: Spec
spec = describe "Civil War team boundaries" do
  it "keeps legacy scenario setup JSON compatible" do
    let old = ScenarioSetup "01094" [] Normal
    fromJSON (object ["scenarioCode" .= ("01094" :: Text), "modulars" .= ([] :: [Text]), "difficulty" .= ("Normal" :: Text)]) `H.shouldBe` Success old
    fromJSON (toJSON old) `H.shouldBe` Success old

  it "round trips custom schemes and both teams without changing the setup's flat shape" do
    let custom = CivilWarScenarioSetup "56059" [] Expert ["56121b","56124b"] (Just setup)
    fromJSON (toJSON custom) `shouldBe` Success custom
    case toJSON custom of
      Object _ -> pure ()
      _ -> expectationFailure "scenario setup must stay an object"

  it "accepts each preconstructed pairing and rejects modules from the wrong faction" do
    CW.validateCompetitionSetup setup `H.shouldBe` Right ()
    let wrong = setup {CW.competitionRegistration = (CW.competitionRegistration setup) {CW.teamScenarioModulars = take 4 CW.resistanceModulars}}
    CW.validateCompetitionSetup wrong `H.shouldBe` Left "error.cwModularFaction"

  it "counts a two-player competition as one player per team" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      getStartingPlayerCount `shouldReturn` 1
      getPlayerCount `shouldReturn` 1
      getPlayers `shouldReturn` [player1 tg]
      asYou (player2 tg) getPlayers `shouldReturn` [player2 tg]
      asYou (player2 tg) getPlayerCount `shouldReturn` 1
      v <- getVillain
      liftIO $ unHp (V.villainMaxHp $ toAttrs v) `shouldBe` 14

  it "uses two-player hit points on each board of a four-player competition" do
    tg <- competitive ["spider-man","she-hulk","captain-marvel","iron-man"]
    inGame tg do
      getStartingPlayerCount `shouldReturn` 2
      v <- getVillain
      liftIO $ unHp (V.villainMaxHp $ toAttrs v) `shouldBe` 28
      second <- addSecondBoard tg
      villains <- getsGame gameVillains
      liftIO $ unHp (V.villainMaxHp $ toAttrs $ villains HashMap.! second) `shouldBe` 24

  it "has a separate first player on each team" do
    tg <- competitive ["spider-man","she-hulk","captain-marvel","iron-man"]
    inGame tg do
      first <- selectJust FirstPlayer
      liftIO $ first `shouldBe` player1 tg
      let resistance = seats tg !! 2
      asYou resistance (selectJust FirstPlayer) `shouldReturn` resistance
      asYou resistance getPlayers `shouldReturn` drop 2 (seats tg)

  it "finishes registration's player phase before resistance begins its turns" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      _ <- addSecondBoard tg
      endTurn
      competition <- getCompetition
      liftIO $ fmap CW.competitivePhase competition `shouldBe` Just CW.ResistancePlayerPhase
      getActivePlayerId `shouldReturn` player2 tg
      getPhasePlayers `shouldReturn` [player2 tg]
      offers (== EndTurn) `shouldReturn` True

  it "plays both villain phases before starting registration's next round" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      _ <- addSecondBoard tg
      roundBefore <- getsGame gameRound
      endTurn
      endTurn
      toFirstTurn
      competition <- getCompetition
      liftIO $ fmap CW.competitivePhase competition `shouldBe` Just CW.RegistrationPlayerPhase
      getActivePlayerId `shouldReturn` player1 tg
      getsGame gameRound `shouldReturn` (roundBefore + 1)

  it "resolves a leader attack with a boost card and no declared defender" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      attacker <- villainIdNow
      defender <- addSecondBoard tg
      _ <- addMinion (player1 tg) M.armoredGuard
      _ <- stackEncounterDeck [S.breakinAndTakin]
      hpBefore <- getsGame $ unHp . V.villainHp . toAttrs . (HashMap.! defender) . gameVillains
      messages <- L.runQueueT $ L.capture $ CWHelp.leaderAttacksLeader attacker defender
      waiting <- openQuestion
      start messages
      openQuestion `shouldReturn` waiting
      hpAfter <- getsGame $ unHp . V.villainHp . toAttrs . (HashMap.! defender) . gameVillains
      liftIO $ hpBefore - hpAfter `shouldBe` 4
      getActivePlayerId `shouldReturn` player1 tg

  it "resolves boost-card 'you' as the defending leader's first player" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      defender <- villainIdNow
      attacker <- addSecondBoard tg
      _ <- stackEncounterDeck [M.hydraHunter,S.breakinAndTakin]
      let threatOnBoard team = do
            schemes <- getMainSchemesIn (CW.teamArea team)
            getsGame $ \g -> sum [Main.getMainSchemeThreat scheme | sid <- schemes, Just scheme <- [lookup sid $ gameMainSchemes g]]
      ownThreat <- threatOnBoard CW.Registration
      otherThreat <- threatOnBoard CW.Resistance
      messages <- L.runQueueT $ L.capture $ CWHelp.leaderAttacksLeader attacker defender
      resolve messages
      threatOnBoard CW.Registration `shouldReturn` ownThreat
      threatOnBoard CW.Resistance `shouldReturn` (otherThreat + 1)
      getActivePlayerId `shouldReturn` player1 tg

  for_ [("answers a leader attack with retaliate",False,1),("respects ranged against a leader's retaliate",True,0)] $ \(description,ranged,expectedDamage) ->
    it description do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        attacker <- villainIdNow
        defender <- addSecondBoard tg
        _ <- stackEncounterDeck [S.breakinAndTakin]
        effects <- L.runQueueT $ L.capture do
          L.lastingModifiers defender (SourceEntity $ VillainRef defender) [KeywordModifier $ K.Retaliate 1] False
          when ranged $ L.lastingModifiers attacker (SourceEntity $ VillainRef attacker) [KeywordModifier K.Ranged] False
        resolve effects
        messages <- L.runQueueT $ L.capture $ CWHelp.leaderAttacksLeader attacker defender
        resolve messages
        villainDamageNow `shouldReturn` expectedDamage

  it "spends stun instead of attacking the other leader" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      attacker <- villainIdNow
      defender <- addSecondBoard tg
      resolve [GiveStatus (VillainRef attacker) StunnedStatus]
      hpBefore <- getsGame $ unHp . V.villainHp . toAttrs . (HashMap.! defender) . gameVillains
      messages <- L.runQueueT $ L.capture $ CWHelp.leaderAttacksLeader attacker defender
      resolve messages
      hpAfter <- getsGame $ unHp . V.villainHp . toAttrs . (HashMap.! defender) . gameVillains
      liftIO $ hpAfter `shouldBe` hpBefore
      enemyMatches StunnedEnemy (EnemyVillainId attacker) `shouldReturn` False

  it "keeps an eliminated team's board separate during the reply" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      second <- addSecondBoard tg
      resolve [IdentityMessage (player2 tg) IdentityDefeated]
      competition <- getCompetition
      liftIO $ fmap (\c -> CW.teamOutcome c CW.Resistance) competition `shouldBe` Just (Just CW.AllHeroesDefeated)
      getsGame gameState `shouldReturn` InProgress
      getActiveVillainIn (CW.teamArea CW.Resistance) `shouldReturn` Just second
      getActiveVillainIn (CW.teamArea CW.Registration) >>= liftIO . (`shouldSatisfy` (/= Just second))
      (length <$> getAreas) `shouldReturn` 2

  it "allows an eliminated first player to make required enemy-team choices" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      resolve [IdentityMessage (player2 tg) IdentityDefeated]
      withGame_ $ questionL .~ mempty
      push $ Ask (player2 tg) $ QuestionLabel "$label.cwEnemyTeamChooses" Nothing $ ChooseOne [Label "$label.done" []]
      runGameMessages
      question <- openQuestion
      liftIO $ fmap fst question `shouldBe` Just (player2 tg)

  it "gives resistance its reply before resolving a registration victory" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      resolve [CardSpecific GameRef "cwFinishBoard" $ toJSON (CW.Registration,CW.EnemyLeaderDefeated)]
      endTurn
      getsGame gameState `shouldReturn` InProgress
      getPhasePlayers `shouldReturn` [player2 tg]
      resolve [CardSpecific GameRef "cwFinishBoard" $ toJSON (CW.Resistance,CW.EnemyLeaderDefeated)]
      endTurn
      getsGame gameState `shouldReturn` Finished CompetitiveDraw

  it "passes only the active team's first-player token" do
    tg <- competitive ["spider-man","she-hulk","captain-marvel","iron-man"]
    inGame tg do
      resolve [PassFirstPlayer]
      let ownNext = seats tg !! 1
          opposingFirst = seats tg !! 2
      selectJust FirstPlayer `shouldReturn` ownNext
      asYou opposingFirst (selectJust FirstPlayer) `shouldReturn` opposingFirst
      competition <- getCompetition
      liftIO $ fmap (\c -> CW.teamFirstPlayer c CW.Registration) competition `shouldBe` Just ownNext
      liftIO $ fmap (\c -> CW.teamFirstPlayer c CW.Resistance) competition `shouldBe` Just opposingFirst

  it "keeps broad enemy and identity matches local, while allowing explicit references" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      otherLeader <- addSecondBoard tg
      enemyMatches AnyEnemy (EnemyVillainId otherLeader) `shouldReturn` False
      enemyMatches (EnemyWithId $ EnemyVillainId otherLeader) (EnemyVillainId otherLeader) `shouldReturn` True
      identityMatches AnyIdentity (player2 tg) `shouldReturn` False
      identityMatches (IdentityWithId $ player2 tg) (player2 tg) `shouldReturn` True

  it "does not offer an opposing hero's basic powers" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      _ <- addSecondBoard tg
      -- The opposing hero's basic powers and form change are never offered here.
      abilities <- getAbilitiesFor (player1 tg)
      liftIO $ map abilitySource abilities `shouldSatisfy` notElem (IdentityRef $ player2 tg)

  it "keeps supports and upgrades in their controller's game area" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      qSupport <- putIntoPlay (player2 tg) "01091"
      qUpgrade <- putIntoPlay (player2 tg) "01093"
      selectListCount (SupportControlledBy AnyIdentity) `shouldReturn` 0
      upgradeMatches (UpgradeWithId $ upgradeOf qUpgrade) (upgradeOf qUpgrade) `shouldReturn` True
      asYou (player2 tg) (selectListCount $ SupportControlledBy $ IdentityWithId $ player2 tg) `shouldReturn` 1
      -- The normal player-facing support list excludes the opponent's card.
      available <- selectList $ SupportControlledBy AnyIdentity
      liftIO $ available `shouldSatisfy` notElem (SupportId $ unCardId $ pcCardId qSupport)

  it "does not apply a side scheme's enemy attack bonus to the opposite board" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      here <- villainIdNow
      other <- addSecondBoard tg
      _ <- revealNow (player1 tg) S.soundTheAlarms
      getModifiers (VillainRef here) >>= (`shouldSatisfy` elem (AttackModifier 1))
      getModifiers (VillainRef other) >>= (`shouldSatisfy` notElem (AttackModifier 1))

  it "keeps Lead from the Front's character bonuses on the acting team's board" do
    tg <- competitive ["spider-man","captain-marvel"]
    let p = player1 tg
    inGame tg do
      _ <- addSecondBoard tg
      changeForm p
      pass
      friendly <- putIntoPlay p "01084"
      opponent <- putIntoPlay (player2 tg) "01083"
      event <- putInHand p "01070"
      resources <- putInHand p "01088"
      playWith event [resources]
      pass
      getModifiers (AllyRef $ AllyId $ unCardId $ pcCardId friendly) >>= (`shouldSatisfy` elem (AttackModifier 1))
      getModifiers (AllyRef $ AllyId $ unCardId $ pcCardId opponent) >>= (`shouldSatisfy` notElem (AttackModifier 1))

  it "does not let an enemy version of Nick Fury prevent the ally from entering play" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      vid <- villainIdNow
      withGame_ $ entitiesL . villainsL . ix vid %~ \(V.Villain v) -> V.Villain $ V.overVillainAttrs
        (\a -> a {V.villainCardDef = (V.villainCardDef a) {cdName = "Nick Fury",cdUnique = True}}) v
      let p = player1 tg
      clearHand p
      card <- putInHand p "01084"
      energy <- putInHand p "01088"
      genius <- putInHand p "01089"
      offers (isPlay card) `shouldReturn` True
      playWith card [energy,genius]
      choose "resolve Nick Fury" (isAbilityOf $ allyIdOf card)
      choose "draw three cards" (isLabel $ countLabel "drawCards" 3)
      length <$> alliesWith "01084" `shouldReturn` 1

  it "prevents playing a unique ally that matches the friendly leader" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      vid <- addSecondBoard tg
      withGame_ $ entitiesL . villainsL . ix vid %~ \(V.Villain v) -> V.Villain $ V.overVillainAttrs
        (\a -> a {V.villainCardDef = (V.villainCardDef a) {cdName = "Nick Fury",cdUnique = True}}) v
      card <- putInHand (player1 tg) "01084"
      _ <- putInHand (player1 tg) "01088"
      _ <- putInHand (player1 tg) "01089"
      offers (isPlay card) `shouldReturn` False

  it "has exactly five physical Standard PvP cards despite the duplicate printings" do
    let total = sum [fromMaybe 0 (cdEncounterSetQuantity def) | def <- C.civilWarDefinitions, cdEncounterSet def == Just E.CwStandardPvp]
    total `H.shouldBe` 5

  for_ (CW.registrationModulars <> CW.resistanceModulars) $ \set ->
    it ("has all five printed cards in " <> show set) do
      sum [fromMaybe 0 (cdEncounterSetQuantity def) | def <- C.civilWarDefinitions, cdEncounterSet def == Just set] `H.shouldBe` 5

  it "does not reveal private search choices to opponents or spectators" do
    tg <- competitive ["spider-man","captain-marvel"]
    let p = player1 tg
    inGame tg do
      card <- putInHand p "01070"
      withGame_ $ questionL .~ HashMap.singleton p (Searched [(Nothing,[PlayerCard card])] $ ChooseOne [CardLabel "01070" $ Label "$label.done" []])
      game <- getGame
      private <- liftIO $ publicGameFor (== p) game
      public <- liftIO $ publicGameFor (const False) game
      let choicesShown value = [length xs | Object root <- [value], Just (Object questions) <- [KeyMap.lookup "question" root], Object question <- KeyMap.elems questions, Just (Array xs) <- [KeyMap.lookup "choices" question]]
          searchesShown value = [length xs | Object root <- [value], Just (Object questions) <- [KeyMap.lookup "question" root], Object question <- KeyMap.elems questions, Just (Array xs) <- [KeyMap.lookup "searched" question]]
      liftIO $ choicesShown private `shouldBe` [1]
      liftIO $ searchesShown private `shouldBe` [1]
      liftIO $ choicesShown public `shouldBe` [0]
      liftIO $ searchesShown public `shouldBe` [0]

  it "preserves the team size and configuration when the game is saved and loaded" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      game <- getGame
      case fromJSON (toJSON game) of
        Error err -> liftIO $ expectationFailure err
        Success restored -> do
          liftIO $ gameCompetition restored `shouldBe` gameCompetition game
          liftIO $ startingPlayerCountIn restored `shouldBe` 1
          view <- liftIO $ publicGameFor (const True) restored
          void $ liftIO $ evaluate (length $ show $ toJSON view)
