-- | Escape the Museum (The Galaxy's Most Wanted, scenario #3): Collector and his Wounded
-- side of infinite hit points, the three main schemes and the Milano set aside and
-- brought back, Library Labyrinth and Museum Ship, and the scenario's treacheries.
module Marvel.Encounter.EscapeTheMuseumSpec (spec) where

import Data.Aeson ((.:))
import Data.Aeson.Types (parseMaybe)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (isInfixOf)
import Marvel.EncounterSet (EncounterSet (ShipCommand))
import Marvel.Environment.Cards qualified as Environments
import Marvel.Game.Public (publicGameFor)
import Marvel.Hp (HP (..))
import Marvel.Phase (Phase (VillainPhase))
import Marvel.Matchers (IdentityMatcher (ConfusedIdentity, HeroIdentity, StunnedIdentity), identityMatches)
import Marvel.Message.Lifted qualified as Lifted

import Marvel.Support.Cards qualified as Supports
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import Marvel.Window qualified as W
import Marvel.Ability.Types (Limit (..))
import TestImport

escape :: CardCode
escape = "16080a"

against :: Difficulty -> [Text] -> IO TestGame
against difficulty heroes = do
  (app, ran, logged) <- dealtGameLogged (ScenarioSetup escape [] difficulty) (map starter heroes) 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

villainCode :: GameAppT CardCode
villainCode = toCardCode . getCardDef <$> getVillain

villainHp :: GameAppT Int
villainHp = (.villainHp) . toAttrs <$> getVillain <&> \(HP n) -> n

damageVillain :: IdentityId -> Natural -> GameAppT ()
damageVillain you n = do
  vid <- villainIdNow
  resolve =<< Lifted.runQueueT (Lifted.capture $ Lifted.dealDamage vid you (toDamage n FromAbility))

environmentCode :: GameAppT CardCode
environmentCode =
  getsGame (HashMap.elems . gameEnvironments) >>= \case
    [e] -> pure (toCardCode (getCardDef e))
    es -> error $ "one environment, not " <> show (length es)

environmentIdNow :: GameAppT EnvironmentId
environmentIdNow = getsGame (fromMaybe (error "no environment") . headMay . HashMap.keys . gameEnvironments)

milanoInPlay :: GameAppT [Attrs Support]
milanoInPlay = supportsWith (toCardCode Supports.milano)

mainSchemeCode :: GameAppT CardCode
mainSchemeCode = toCardCode . getCardDef <$> (getsGame (fromMaybe (error "no main scheme") . headMay . HashMap.elems . gameMainSchemes))

nextStage :: GameAppT ()
nextStage = mainSchemeIdNow >>= \m -> resolve [NextMainScheme m]

asHero :: IdentityId -> GameAppT ()
asHero you = identityMatches HeroIdentity you >>= \h -> unless h (changeForm you)

asAlterEgo :: IdentityId -> GameAppT ()
asAlterEgo you = identityMatches HeroIdentity you >>= \h -> when h (changeForm you)

removeMainThreat :: IdentityId -> Natural -> GameAppT ()
removeMainThreat you n = do
  m <- mainSchemeIdNow
  resolve =<< Lifted.runQueueT (Lifted.capture $ Lifted.thwartScheme m you n)

infiniteShown :: GameAppT (Maybe Bool)
infiniteShown = do
  view <- liftIO . publicGameFor (const True) =<< getGame
  pure $ flip parseMaybe view \v -> do
    o <- parseJSON v
    villains <- o .: "villains"
    case villains of
      [villain] -> villain .: "infiniteHitPoints"
      _ -> fail "one villain"

spec :: Spec
spec = describe "Escape the Museum" do
  describe "setup" do
    it "Collector (A1) against The Missing Milano, Library Labyrinth in play, the Ship Command set set aside" do
      tg <- against Normal ["spider-man"]
      inGame tg do
        villainCode `shouldReturn` "16080a"
        mainSchemeCode `shouldReturn` "16082"
        mainSchemeThreatNow `shouldReturn` 7
        environmentCode `shouldReturn` toCardCode Environments.libraryLabyrinth
        milanoInPlay >>= liftIO . (`shouldBe` 0) . length
        deck <- scenarioEncounterDeckNow
        aside <- getSetAsideEncounterCards
        liftIO do
          any ((== Just ShipCommand) . cdEncounterSet . getCardDef) deck `shouldBe` False
          any ((== Supports.milano) . getCardDef) aside `shouldBe` True
          length (filter ((== Just ShipCommand) . cdEncounterSet . getCardDef) aside) `shouldBe` 7

    it "Collector (B1) for expert play: 2 SCH and 2 ATK, +1 each at stage 1" do
      tg <- against Expert ["spider-man"]
      inGame tg do
        villainCode `shouldReturn` "16081a"
        a <- toAttrs <$> getVillain
        sch <- Villain.getModifiedScheme a
        atk <- Villain.getModifiedAttack a
        liftIO $ (sch, atk) `shouldBe` (3, 3)
        damageVillain (player1 tg) 10
        villainCode `shouldReturn` "16081b"
        resolve [CheckWindows [W.Window W.When W.RoundEnded]]
        villainCode `shouldReturn` "16081a"
        villainHp `shouldReturn` 10

  describe "Collector" do
    it "gets +X SCH and +X ATK, X the main scheme's stage number" do
      tg <- against Normal ["spider-man"]
      inGame tg do
        let stats = do
              a <- toAttrs <$> getVillain
              (,) <$> Villain.getModifiedScheme a <*> Villain.getModifiedAttack a
        stats `shouldReturn` (2, 2)
        nextStage
        stats `shouldReturn` (3, 3)
        nextStage
        stats `shouldReturn` (4, 4)

    it "would be defeated: 3 threat a player removed from the main scheme and he flips to his Wounded side instead" do
      tg <- against Normal ["spider-man"]
      let you = player1 tg
      inGame tg do
        before <- mainSchemeThreatNow
        damageVillain you 8
        villainCode `shouldReturn` "16080b"
        mainSchemeThreatNow `shouldReturn` (before - 3)
        getsGame gameState `shouldReturn` InProgress
        infiniteShown `shouldReturn` Just True
      -- not defeated: no defeat told
      logged <- readIORef tg.logged
      map entryText logged `shouldSatisfy` all (not . ("$log.defeated" `isInfixOf`))

    it "Wounded, he has infinite hit points: damage is dealt to him but he is not defeated, and has no stats" do
      tg <- against Normal ["spider-man"]
      let you = player1 tg
      inGame tg do
        damageVillain you 8
        damageVillain you 30
        villainCode `shouldReturn` "16080b"
        getsGame gameState `shouldReturn` InProgress
        a <- toAttrs <$> getVillain
        sch <- Villain.getModifiedScheme a
        atk <- Villain.getModifiedAttack a
        liftIO $ (sch, atk) `shouldBe` (0, 0)

    it "Wounded: as the round ends he flips back, his hit point dial at his printed hit points" do
      tg <- against Normal ["spider-man"]
      let you = player1 tg
      inGame tg do
        infiniteShown `shouldReturn` Just False
        damageVillain you 8
        resolve [CheckWindows [W.Window W.When W.RoundEnded]]
        villainCode `shouldReturn` "16080a"
        villainHp `shouldReturn` 8
        villainDamageNow `shouldReturn` 0

    it "not Wounded, the round's end does not flip him" do
      tg <- against Normal ["spider-man"]
      let you = player1 tg
      inGame tg do
        damageVillain you 3
        resolve [CheckWindows [W.Window W.When W.RoundEnded]]
        villainCode `shouldReturn` "16080a"
        villainDamageNow `shouldReturn` 3

  describe "main schemes" do
    it "The Missing Milano: the last threat removed, Lost in the Museum, the Milano under the first player's control" do
      tg <- against Normal ["spider-man", "she-hulk"]
      let you = player1 tg
      inGame tg do
        removeMainThreat you 13
        mainSchemeCode `shouldReturn` "16082"
        removeMainThreat you 1
        mainSchemeCode `shouldReturn` "16083"
        mainSchemeThreatNow `shouldReturn` 22
        milanos <- milanoInPlay
        supports <- (.playerIdentitySupports) <$> playerAttrs you
        aside <- getSetAsideEncounterCards
        liftIO do
          map (.supportController) milanos `shouldBe` [you]
          map supportId milanos `shouldSatisfy` all (`elem` supports)
          any ((== Supports.milano) . getCardDef) aside `shouldBe` False

    it "The Missing Milano completed: the players lose" do
      tg <- against Normal ["spider-man"]
      inGame tg do
        placeMainThreat 4
        getsGame gameState `shouldReturn` Finished Lost

    it "Lost in the Museum completed: the players lose" do
      tg <- against Normal ["spider-man"]
      inGame tg do
        nextStage
        placeMainThreat 4
        getsGame gameState `shouldReturn` Finished Lost

    it "The Great Escape: Museum Ship, an acceleration token and the rest of Ship Command shuffled in" do
      tg <- against Normal ["spider-man"]
      let you = player1 tg
      inGame tg do
        nextStage
        tokens <- accelerationTokensNow
        removeMainThreat you 11
        mainSchemeCode `shouldReturn` "16084"
        environmentCode `shouldReturn` toCardCode Environments.museumShip
        accelerationTokensNow `shouldReturn` (tokens + 1)
        deck <- scenarioEncounterDeckNow
        aside <- getSetAsideEncounterCards
        liftIO do
          length (filter ((== Just ShipCommand) . cdEncounterSet . getCardDef) deck) `shouldBe` 6
          any ((== Just ShipCommand) . cdEncounterSet . getCardDef) aside `shouldBe` False

    it "The Great Escape: First Player Action, exhaust the Milano → remove 3 threat; with none left the players win" do
      tg <- against Normal ["spider-man"]
      let you = player1 tg
      inGame tg do
        nextStage
        nextStage
        before <- mainSchemeThreatNow
        scheme <- mainSchemeIdNow
        choose "the scheme's action" (isAbilityOf scheme)
        mainSchemeThreatNow `shouldReturn` (before - 3)
        milanoInPlay >>= liftIO . (`shouldBe` [True]) . map (.supportExhausted)
        offers (isAbilityOf scheme) `shouldReturn` False
        removeMainThreat you 5
        getsGame gameState `shouldReturn` Finished Won

    it "The Great Escape completed: the players lose" do
      tg <- against Normal ["spider-man"]
      inGame tg do
        nextStage
        nextStage
        placeMainThreat 4
        getsGame gameState `shouldReturn` Finished Lost

    it "Collector's defeat removing the last threat advances the stage" do
      tg <- against Normal ["spider-man"]
      let you = player1 tg
      inGame tg do
        removeMainThreat you 4
        damageVillain you 8
        mainSchemeCode `shouldReturn` "16083"

  describe "Library Labyrinth" do
    it "Hero Action: deal yourself an encounter card facedown → remove 5 threat from the main scheme, once a round" do
      tg <- against Normal ["spider-man"]
      let you = player1 tg
      inGame tg do
        asHero you
        placeMainThreat 3
        env <- environmentIdNow
        before <- mainSchemeThreatNow
        dealt <- length <$> dealtTo you
        choose "This way?" (isAbilityOf env)
        mainSchemeThreatNow `shouldReturn` (before - 5)
        (length <$> dealtTo you) `shouldReturn` (dealt + 1)
        offers (isAbilityOf env) `shouldReturn` False

    it "once a round for each player: the next player may use it too" do
      tg <- against Normal ["spider-man", "she-hulk"]
      let (you, them) = (player1 tg, player2 tg)
      inGame tg do
        asHero you
        pass
        env <- environmentIdNow
        choose "This way?" (isAbilityOf env)
        offers (isAbilityOf env) `shouldReturn` False
        endTurn
        asHero them
        pass
        placeMainThreat 5
        before <- mainSchemeThreatNow
        dealt <- length <$> dealtTo them
        choose "This way?, the other player" (isAbilityOf env)
        mainSchemeThreatNow `shouldReturn` (before - 5)
        (length <$> dealtTo them) `shouldReturn` (dealt + 1)
        offers (isAbilityOf env) `shouldReturn` False

    it "its limit is each player's; a plain limit on an encounter card would be the table's" do
      tg <- against Normal ["spider-man", "she-hulk"]
      let (you, them) = (player1 tg, player2 tg)
      inGame tg do
        env <- environmentIdNow
        labyrinth <- fromMaybe (error "no ability") . headMay . filter ((== toSource env) . abilitySource) <$> getAbilitiesFor you
        let plain = labyrinth {abilityLimit = PerRound 1}
        resolve [UsedAbility you labyrinth]
        withinUseLimit you labyrinth `shouldReturn` False
        withinUseLimit them labyrinth `shouldReturn` True
        withinUseLimit them plain `shouldReturn` False

    it "is no alter-ego action" do
      tg <- against Normal ["spider-man"]
      let you = player1 tg
      inGame tg do
        asAlterEgo you
        env <- environmentIdNow
        offers (isAbilityOf env) `shouldReturn` False

  describe "Museum Ship" do
    it "as the villain phase begins: exhaust the Milano → 2 indirect damage a player among the players" do
      tg <- against Normal ["spider-man", "she-hulk"]
      let (you, them) = (player1 tg, player2 tg)
      inGame tg do
        nextStage
        nextStage
        asAlterEgo you
        asAlterEgo them
        before <- damageOf them
        start [BeginPhase VillainPhase]
        offers (isLabel (countLabel "indirectDamageAmongPlayers" 6)) `shouldReturn` True
        choose "exhaust the Milano" (isLabel (countLabel "exhaustTheMilanoIndirectDamageAmongPlayers" 4))
        -- the first player assigns it, to either player's characters
        for_ [1 .. 4 :: Int] \_ -> choose "the second player" (isTargetChoice (IdentityCharacter them))
        damageOf them `shouldReturn` (before + 4)
        milanoInPlay >>= liftIO . (`shouldBe` [True]) . map (.supportExhausted)

    it "with the Milano exhausted, 3 indirect damage a player" do
      tg <- against Normal ["spider-man"]
      let you = player1 tg
      inGame tg do
        nextStage
        nextStage
        asAlterEgo you
        milano <- milanoInPlay
        resolve [SupportMessage (supportId m) ExhaustedSupport | m <- milano]
        before <- damageOf you
        start [BeginPhase VillainPhase]
        damageOf you `shouldReturn` (before + 3)

  describe "treacheries" do
    it "\"I Have You Now!\" (hero): you are stunned and Collector attacks you" do
      tg <- against Normal ["spider-man"]
      let you = player1 tg
      inGame tg do
        asHero you
        _ <- stackEncounterDeck [Treacheries.specialDelivery]
        before <- damageOf you
        _ <- revealNow you Treacheries.iHaveYouNow
        chooseIf isNoResponse
        pressWhenRevealed
        chooseIf isNoResponse
        choose "no defender" isNoDefender
        chooseIf isNoResponse
        stunned <- identityMatches StunnedIdentity you
        after <- damageOf you
        liftIO do
          stunned `shouldBe` True
          after `shouldBe` before + 2

    it "\"I Have You Now!\" (alter-ego): your identity is exhausted and Collector schemes" do
      tg <- against Normal ["spider-man"]
      let you = player1 tg
      inGame tg do
        asAlterEgo you
        _ <- stackEncounterDeck [Treacheries.specialDelivery]
        before <- mainSchemeThreatNow
        _ <- revealNow you Treacheries.iHaveYouNow
        resolve []
        exhaustedNow you `shouldReturn` True
        mainSchemeThreatNow `shouldReturn` (before + 2)

    it "\"I Have You Now!\" boost: Collector is given a tough status card" do
      tg <- against Normal ["spider-man"]
      let you = player1 tg
      inGame tg do
        asAlterEgo you
        _ <- stackEncounterDeck [Treacheries.iHaveYouNow]
        vid <- villainIdNow
        resolve [VillainMessage vid VillainSchemes]
        (Villain.villainTough . toAttrs <$> getVillain) `shouldReturn` True

    it "Impossible Geometry: incite 1 and you are confused" do
      tg <- against Normal ["spider-man"]
      let you = player1 tg
      inGame tg do
        before <- mainSchemeThreatNow
        _ <- revealNow you Treacheries.impossibleGeometry
        resolve []
        mainSchemeThreatNow `shouldReturn` (before + 1)
        identityMatches ConfusedIdentity you `shouldReturn` True

    it "Impossible Geometry, already confused: choose and discard a card you control, not the permanent Milano" do
      tg <- against Normal ["spider-man"]
      let you = player1 tg
      inGame tg do
        nextStage
        ally <- putIntoPlay you "01002" -- Black Cat: an ally
        resolve [IdentityMessage you IdentityConfused]
        milano <- milanoInPlay
        _ <- revealNow you Treacheries.impossibleGeometry
        offers (\c -> any (\m -> isTargetChoice (supportId m) c) milano) `shouldReturn` False
        chooseIf (isTargetChoice (allyIdOf ally))
        alliesWith "01002" >>= liftIO . (`shouldBe` 0) . length
