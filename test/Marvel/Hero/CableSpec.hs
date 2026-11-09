module Marvel.Hero.CableSpec (spec) where

import Data.Maybe (fromJust)
import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Game.Public (publicGameFor)
import Marvel.Trait (Trait (..))
import Marvel.Window qualified as W
import Marvel.Scenario.Types (scenarioEncounterDeck)
import Data.HashMap.Strict qualified as HashMap
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.SideScheme.Types qualified as SideScheme
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity ( currentIdentity )
import Marvel.Modifier
import Marvel.Query
import Marvel.Matchers hiding (ExhaustedIdentity)
import TestImport


asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

cardAbility :: PlayerCard -> Natural -> Choice -> Bool
cardAbility card n choice = isAbilityOfCard card choice && case choice of
  UseAbility ab -> abilityIndex ab == n
  _ -> False

purge :: GameAppT (Maybe SideSchemeId)
purge = getsGame $ headMay . HashMap.keys . HashMap.filter ((== "40006") . toCardCode . getCardDef) . gameSideSchemes

-- Individual card tests explicitly arrange Purge rather than depend on the
-- setup search's first offered card or on the shuffled opening hand.
cableGame :: [Text] -> IO TestGame
cableGame heroes = do
  tg <- testGame heroes
  inGame tg do
    current <- purge
    when (isNothing current) do
      sides <- getsGame $ HashMap.keys . HashMap.filter (isJust . SideScheme.sideSchemePlayerCard . toAttrs) . gameSideSchemes
      resolve [RemoveFromPlay $ SideSchemeRef sid | sid <- sides]
      _ <- putIntoPlay (player1 tg) "40006"
      pure ()
  pure tg

spec :: Spec
spec = describe "Cable" do
  it "Nathan has 12 HP, 6 hand size and starts with a player side scheme" do
    tg <- testGame ["cable"]
    inGame tg do
      let p = player1 tg
      hitPointsOf p `shouldReturn` 12
      handSizeOf p `shouldReturn` 6
      getsGame (any (isJust . SideScheme.sideSchemePlayerCard . toAttrs) . HashMap.elems . gameSideSchemes) `shouldReturn` True

  it "puts the side scheme chosen during setup straight into play without asking twice" do
    let setup = fromMaybe (error "no Rhino setup") (recommendedSetup "01094")
    (app, ran, logged) <- dealtGameLogged setup [starter "cable"] 1
    seats <- runGameApp app (getsGame gameSeats)
    let tg = TestGame app ran logged seats
    inGame tg do
      let setupChoices = do
            attrs <- playerAttrs (player1 tg)
            let schemes = [pcCardId c | c <- unDeck (playerIdentityDeck attrs) <> unDiscard (playerIdentityDiscard attrs), cdCardType (getCardDef c) == PlayerSideSchemeType]
            options <- maybe [] snd <$> asked
            pure [cid | TargetLabel (CardIdRef cid) _ <- options, cid `elem` schemes]
          advance 0 = error "setup did not reach Cable's search"
          advance n = do
            selecting <- notNull <$> setupChoices
            unless selecting do
              asked >>= \case
                Just (ident, _) -> answerQuestion ident 0 >> advance (n - 1)
                Nothing -> error "no setup question"
      advance (100 :: Int)
      chosen <- setupChoices >>= maybe (error "missing side scheme choice") pure . headMay
      choose "side scheme from Cable's search" (isTargetChoice $ CardIdRef chosen)
      getsGame (HashMap.member (SideSchemeId $ unCardId chosen) . gameSideSchemes) `shouldReturn` True
      offers (isTargetChoice $ CardIdRef chosen) `shouldReturn` False
      view <- getGame >>= liftIO . publicGameFor (== player1 tg)
      let owners = case view of
            Object object -> case KeyMap.lookup "sideSchemes" object of
              Just (Array schemes) -> [KeyMap.lookup "owner" fields | Object fields <- toList schemes, KeyMap.lookup "id" fields == Just (toJSON chosen)]
              _ -> []
            _ -> []
      owners `shouldBe` [Just $ toJSON $ player1 tg]
      toFirstTurn

  it "Technovirus Purge permits Cable and restricts other players' threat removal" do
    tg <- cableGame ["cable", "spider-man"]
    inGame tg do
      sid <- fromJust <$> purge
      mods <- getModifiers (SideSchemeRef sid)
      mods `shouldSatisfy` (\actual -> all (`elem` actual) [ThreatRemovableOnlyByCharacter $ IdentityCharacter $ player1 tg])

  it "Technovirus Purge grants Cable one to each basic stat while in victory" do
    tg <- cableGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      purge >>= traverse_ (\sid -> resolve [AddToVictoryDisplay (SideSchemeRef sid)])
      currentIdentity <$> getPlayer p >>= \case
        HeroSide hero -> do
          Hero.getModifiedThwart (toAttrs hero) `shouldReturn` 3
          Hero.getModifiedAttack (toAttrs hero) `shouldReturn` 3
          Hero.getModifiedDefense (toAttrs hero) `shouldReturn` 3
        _ -> error "Cable should be a hero"
      selectAny (IdentityWithId p <> IdentityWithTrait Psionic) `shouldReturn` True

  it "Bodyslide changes Cable and lets another player adopt his new form" do
    tg <- cableGame ["cable", "spider-man"]
    inGame tg do
      let p = player1 tg
      clearHand p
      slide <- putInHand p "40002"
      playWith slide []
      choose "change the other player's form" (isLabel "$label.changeForm")
      pass
      selectAny (IdentityWithId p <> HeroIdentity) `shouldReturn` True
      selectAny (IdentityWithId (player2 tg) <> HeroIdentity) `shouldReturn` True

  it "Mind Scan counts player side schemes in the victory display" do
    tg <- cableGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      sid <- fromJust <$> purge
      resolve [AddToVictoryDisplay $ SideSchemeRef sid]
      main <- mainSchemeIdNow
      resolve [MainSchemeMessage main $ MainSchemePlaceThreat 5]
      scan <- putInHand p "40003"
      genius <- putInHand p "01089"
      playWith scan [genius]
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` 1

  it "Telekinetic Blast adds damage for a player side scheme in victory" do
    tg <- cableGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      sid <- fromJust <$> purge
      resolve [AddToVictoryDisplay $ SideSchemeRef sid]
      blast <- putInHand p "40005"
      energy <- putInHand p "01088"
      extra <- putInHand p "01091"
      playWith blast [energy, extra]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 7

  it "Precognition may discard the looked-at encounter card" do
    tg <- cableGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      sid <- fromJust <$> purge
      resolve [AddToVictoryDisplay $ SideSchemeRef sid]
      top <- testHead <$> stackEncounterDeck [Treacheries.advance]
      precognition <- putInHand p "40004"
      playWith precognition []
      choose "discard the looked-at card" (isTargetChoice top)
      pass
      encounterDiscard >>= \cards -> cards `shouldSatisfy` elem top

  it "Precognition resolves safely when the victory display has no schemes" do
    tg <- cableGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      before <- getsGame $ scenarioEncounterDeck . toAttrs . gameScenario
      precognition <- putInHand p "40004"
      playWith precognition []
      pass
      getsGame (scenarioEncounterDeck . toAttrs . gameScenario) `shouldReturn` before

  it "Cable readies after he defeats a side scheme" do
    tg <- cableGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      sid <- fromJust <$> purge
      resolve [IdentityMessage p ExhaustedIdentity]
      runNow [CheckWindows [W.Window W.After $ W.DefeatedSideSchemeBy sid p (Just $ IdentityRef p)]]
      choose "Cable ready response" (\case UseAbility ab -> abilitySource ab == IdentityRef p; _ -> False)
      pass
      isExhaustedNow p `shouldReturn` False

  it "Graymalkin generates an energy resource and exhausts when paying" do
    tg <- cableGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      gray <- putIntoPlay p "40007"
      card <- putInHand p "40009"
      choose "play Askani'son" (isPlay card)
      choose "Graymalkin resource" (cardAbility gray 2)
      whenM (offers isFinishPayment) $ choose "finish payment" isFinishPayment
      pass
      supportExhaustedNow (supportOf gray) `shouldReturn` True

  for_ [False, True] $ \heroExhausted ->
    for_ [False, True] $ \grayExhausted ->
      it ("Graymalkin's response checks its own exhaustion; hero=" <> show heroExhausted <> ", support=" <> show grayExhausted) do
        tg <- cableGame ["cable"]
        inGame tg do
          let p = player1 tg
          asHero p
          sid <- fromJust <$> purge
          gray <- putIntoPlay p "40007"
          when heroExhausted $ runNow [IdentityMessage p ExhaustedIdentity]
          when grayExhausted $ runNow [SupportMessage (supportOf gray) ExhaustedSupport]
          runNow [CheckWindows [W.Window W.After $ W.DefeatedSideSchemeBy sid p (Just $ IdentityRef p)]]
          offers (cardAbility gray 1) `shouldReturn` grayExhausted
          when grayExhausted do
            choose "ready Graymalkin" (cardAbility gray 1)
            supportExhaustedNow (supportOf gray) `shouldReturn` False
          pass

  it "Professor offers an alter ego draw and exhausts" do
    tg <- cableGame ["cable"]
    inGame tg do
      let p = player1 tg
      clearHand p
      professor <- putIntoPlay p "40008"
      before <- length <$> handOf p
      choose "Professor" (isAbilityOfCard professor)
      choose "draw" (isLabel "$label.drawOneCard")
      pass
      length <$> handOf p `shouldReturn` before + 1
      supportExhaustedNow (supportOf professor) `shouldReturn` True

  it "Professor cannot be used in hero form" do
    tg <- cableGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      professor <- putIntoPlay p "40008"
      offers (isAbilityOfCard professor) `shouldReturn` False

  it "Plasma Rifle is unavailable in alter ego form" do
    tg <- cableGame ["cable"]
    inGame tg do
      rifle <- putIntoPlay (player1 tg) "40011"
      offers (isAbilityOfCard rifle) `shouldReturn` False

  it "Mission Leader draws one card for every player and exhausts" do
    tg <- cableGame ["cable", "spider-man"]
    inGame tg do
      let p = player1 tg
      asHero p
      leader <- putIntoPlay p "40023"
      before <- length <$> handOf p
      sid <- fromJust <$> purge
      runNow [SideSchemeMessage sid DefeatSideScheme, DefeatedLeavesPlay (SideSchemeRef sid), CheckWindows [W.Window W.After $ W.DefeatedSideSchemeBy sid p (Just $ IdentityRef p)]]
      choose "Mission Leader" (isAbilityOfCard leader)
      pass
      length <$> handOf p `shouldReturn` before + 1
      upgradeExhaustedNow (upgradeIdOf leader) `shouldReturn` True

  it "Temporal Leap pays by restoring a victory scheme before moving existing threat" do
    tg <- cableGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      sid <- fromJust <$> purge
      resolve [AddToVictoryDisplay (SideSchemeRef sid)]
      leap <- putIntoPlay p "40013"
      main <- mainSchemeIdNow
      resolve [MainSchemeMessage main $ MainSchemePlaceThreat 6]
      runNow [MainSchemeMessage main $ MainSchemePlaceThreat 2]
      choose "Temporal Leap" (isAbilityOfCard leap)
      choose "restore Purge" (isTargetChoice $ CardIdRef $ CardId $ unSideSchemeId sid)
      pass
      mainSchemeThreatNow `shouldReturn` 4
      sideSchemeThreat sid `shouldReturn` 9
      getsGame (HashMap.member (upgradeIdOf leap) . gameUpgrades) `shouldReturn` False
      map toCardCode <$> getPlayerVictoryDisplay `shouldReturn` []

  it "Telekinetic Force Field cannot be played in alter ego form" do
    tg <- cableGame ["cable"]
    inGame tg do
      let p = player1 tg
      clearHand p
      field <- putInHand p "40012"
      _ <- putInHand p "01088"
      offers (isPlay field) `shouldReturn` False

  it "Askani'son waits for a defended attack" do
    tg <- cableGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      askani <- putIntoPlay p "40009"
      _ <- putInHand p "01088"
      offers (isAbilityOfCard askani) `shouldReturn` False

  it "Forced Amnesia cannot trigger without a newly defeated scheme" do
    tg <- cableGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      amnesia <- putIntoPlay p "40010"
      offers (isAbilityOfCard amnesia) `shouldReturn` False

supportExhaustedNow :: SupportId -> GameAppT Bool
supportExhaustedNow sid = getsGame $ maybe False (supportExhausted . toAttrs) . HashMap.lookup sid . gameSupports

upgradeExhaustedNow :: UpgradeId -> GameAppT Bool
upgradeExhaustedNow uid = getsGame $ maybe False (upgradeExhausted . toAttrs) . HashMap.lookup uid . gameUpgrades
