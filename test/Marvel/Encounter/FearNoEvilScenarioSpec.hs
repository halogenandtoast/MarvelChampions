module Marvel.Encounter.FearNoEvilScenarioSpec (spec) where

import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Query
import Marvel.Trait qualified as T
import Marvel.Keyword (Keyword(Guard))
import Marvel.Phase (Phase(VillainPhase))
import Marvel.Window qualified as W
import Data.List (findIndex)
import TestImport

scenarioGame :: CardCode -> Difficulty -> [Text] -> IO TestGame
scenarioGame code mode heroes = scenarioGameChoosing code mode heroes Nothing

scenarioGameChoosing :: CardCode -> Difficulty -> [Text] -> Maybe CardCode -> IO TestGame
scenarioGameChoosing code mode heroes preferred = do
  let setup = (fromMaybe (error "scenario missing") $ recommendedSetup code) {difficulty=mode}
  (app,ran,logged) <- dealtGameWith id setup (map starter heroes) 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  tg <$ inGame tg (setupChoices (200 :: Int))
 where
  setupChoices 0 = error "scenario setup did not finish"
  setupChoices n = openQuestion >>= \case
    Just (_,q) | EndTurn `elem` choicesOf q -> pure ()
    Just (you,q) -> do
      let matches = \case CardLabel code _ -> Just code == preferred; _ -> False
      answerQuestion you $ fromMaybe 0 $ findIndex matches $ choicesOf q
      setupChoices (n-1)
    Nothing -> error "scenario stopped during setup"

spec :: Spec
spec = describe "Fear No Evil scenarios" do
  it "Public Support advances Kingpin and the main scheme after four defeated minions' support counters" do
    tg <- scenarioGame "60161a" Normal ["spider-man"]
    inGame tg do
      eid <- selectJust $ EnvironmentIs Cards.street60163a
      resolve =<< L.runQueueT (L.capture $ L.placeCounters eid 3)
      minion <- selectJust AnyMinion
      resolve [MinionMessage minion $ MinionDamaged GameRef $ toDamage 20 FromAbility]
      getCountersOn eid `shouldReturn` 4
      resolve [CheckWindows [W.Window W.When $ W.PhaseBegan VillainPhase]]
      selectAny (VillainWithCardCode "60159b") `shouldReturn` True
      selectAny (EnvironmentIs Cards.street60163a) `shouldReturn` False
      selectAny (EnvironmentIs Cards.street60163b) `shouldReturn` True
      SchemeMainSchemeId main <- selectJust EncounterMainScheme
      getMainSchemeCode main `shouldReturn` Just "60162b"
      minions <- selectList AnyMinion
      null minions `shouldBe` False
      for_ minions $ \mid -> minionMatches (MinionWithKeyword Guard) mid `shouldReturn` True

  it "high Public Support spends a support counter to cancel a boost before the deck card is assigned" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      card <- encounterCard Cards.street60163b
      resolve =<< L.runQueueT (L.capture $ L.putEncounterCardIntoPlay you card)
      let eid = EnvironmentId $ unCardId $ ecCardId card
      resolve =<< L.runQueueT (L.capture $ L.placeCounters eid 1)
      (deck,_) <- setEncounterDeck (replicate 3 Cards.street60188) []
      vid <- villainIdNow
      runNow [VillainMessage vid $ VillainSchemesAgainst you]
      choose "spend Public Support" (isAbilityOf eid)
      pass
      getCountersOn eid `shouldReturn` 0
      getEncounterDeck `shouldReturn` deck
      mainSchemeThreatNow `shouldReturn` 1

  it "The King's Gambit action works repeatedly with an empty discard and zero remaining threat" do
    tg <- scenarioGame "60161a" Normal ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      SchemeMainSchemeId main <- selectJust EncounterMainScheme
      _ <- setEncounterDeck [Cards.street60092,Cards.street60093,Cards.street60188,Cards.street60188] []
      choose "use the first Gambit action" (isAbilityOf main)
      pass
      mainSchemeThreatNow `shouldReturn` 0
      selectListCount AnyMinion `shouldReturn` 2
      offers (isAbilityOf main) `shouldReturn` True
      choose "use the action again at zero threat" (isAbilityOf main)
      pass
      selectAny (MinionIs Cards.street60092) `shouldReturn` True
      selectAny (MinionIs Cards.street60093) `shouldReturn` True
      mainSchemeThreatNow `shouldReturn` 0

  it "Hung Out to Dry damages allies and minions entering its area, without affecting the other player's area" do
    tg <- scenarioGameChoosing "60134a" Normal ["spider-man","she-hulk"] (Just "60136b")
    inGame tg do
      let you = player1 tg
      Just sid <- getOwnedMainScheme you
      _ <- putIntoPlay you "01050"
      allyDamage <$> allyIn "01050" `shouldReturn` 1
      mid <- addMinion you Cards.street60130
      minionDamageOf mid `shouldReturn` Just 1
      _ <- putIntoPlay (player2 tg) "01002"
      allyDamage <$> allyIn "01002" `shouldReturn` 0
      getSchemeThreatAmount (SchemeMainSchemeId sid) `shouldReturn` 2

  it "Bull in a China Shop counts excess damage to an ally but not a minion in normal mode" do
    tg <- scenarioGameChoosing "60134a" Normal ["spider-man"] (Just "60135b")
    inGame tg do
      let you = player1 tg
      Just sid <- getOwnedMainScheme you
      ally <- putIntoPlay you "01002"
      resolve [AllyMessage (allyIdOf ally) $ AllyDamaged GameRef $ toDamage 5 FromAbility]
      getSchemeThreatAmount (SchemeMainSchemeId sid) `shouldReturn` 3
      mid <- addMinion you Cards.street60092
      resolve [MinionMessage mid $ MinionDamaged GameRef $ toDamage 5 FromAbility]
      getSchemeThreatAmount (SchemeMainSchemeId sid) `shouldReturn` 3

  it "A Slice of the Pizza Dough heals its attacking hero after defeating a minion in another player's area" do
    tg <- scenarioGameChoosing "60134a" Normal ["spider-man","she-hulk"] (Just "60138b")
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      Just sid <- getOwnedMainScheme you
      resolve =<< L.runQueueT (L.capture $ L.damageIdentity you GameRef $ toDamage 3 FromAbility)
      mid <- addMinion (player2 tg) Cards.street60092
      resolve [MinionMessage mid $ MinionDamaged GameRef $ toDamage 1 FromAbility]
      choose "attack the wounded Enforcer" (isHeroAttack you)
      choose "target the other player's minion" (isTargetChoice mid)
      choose "heal through the pizzeria" (isAbilityOf sid)
      pass
      damageOf you `shouldReturn` 2
      getSchemeThreatAmount (SchemeMainSchemeId sid) `shouldReturn` 1

  it "Shop Proprietor's leave-play penalty affects only the main scheme in its controller's area" do
    tg <- scenarioGameChoosing "60134a" Normal ["spider-man","she-hulk"] (Just "60135b")
    inGame tg do
      let you = player1 tg
      Just own <- getOwnedMainScheme you
      Just other <- getOwnedMainScheme (player2 tg)
      _ <- setEncounterDeck (replicate 4 Cards.street60188) []
      card <- encounterCard Cards.street60139
      resolve [RevealEncounterCard you card]
      let aid = AllyId $ unCardId $ ecCardId card
      selectAny (AllyWithId aid) `shouldReturn` True
      resolve [RemoveFromPlay $ AllyRef aid]
      getSchemeThreatAmount (SchemeMainSchemeId own) `shouldReturn` 4
      getSchemeThreatAmount (SchemeMainSchemeId other) `shouldReturn` 0

  it "Man-Bull places threat equal to excess attack damage on its area's main scheme" do
    tg <- scenarioGameChoosing "60134a" Normal ["spider-man","she-hulk"] (Just "60137b")
    inGame tg do
      let you = player1 tg
      Just own <- getOwnedMainScheme you
      Just other <- getOwnedMainScheme (player2 tg)
      ally <- putIntoPlay you "01002"
      mid <- addMinion you Cards.street60140
      resolve [MinionMessage mid $ MinionAttacksAlly you $ allyIdOf ally]
      getSchemeThreatAmount (SchemeMainSchemeId own) `shouldReturn` 2
      getSchemeThreatAmount (SchemeMainSchemeId other) `shouldReturn` 0

  it "Change of Venue's boost preserves the business's threat and swaps without resolving surge" do
    tg <- scenarioGameChoosing "60134a" Normal ["spider-man","she-hulk"] (Just "60135b")
    inGame tg do
      let you = player1 tg
      Just previous <- getOwnedMainScheme you
      resolve =<< L.runQueueT (L.capture $ L.placeThreatOn GameRef previous 4)
      _ <- setEncounterDeck (Cards.street60141 : replicate 7 Cards.street60188) []
      vid <- villainIdNow
      resolve [VillainMessage vid $ VillainSchemesAgainst you]
      Just current <- getOwnedMainScheme you
      (current /= previous) `shouldBe` True
      getSchemeThreatAmount (SchemeMainSchemeId current) `shouldReturn` 6
      length <$> getEncounterDeck `shouldReturn` 7
      selectListCount EncounterMainScheme `shouldReturn` 2

  it "Pawn Shop discounts another player's upgrade played on an ally in its area and consumes that area's first upgrade" do
    tg <- scenarioGameChoosing "60134a" Normal ["spider-man","she-hulk"] (Just "60137b")
    inGame tg do
      let first = player1 tg
          second = player2 tg
      ally <- putIntoPlay first "01002"
      _ <- putIntoPlay first "01050"
      choose "end the first player's turn" (== EndTurn)
      changeForm second
      pass
      clearHand second
      inspired <- putInHand second "01074"
      playOn inspired (allyIdOf ally) []
      main <- getOwnedMainScheme first
      main `shouldSatisfy` isJust
      for_ main $ \sid -> choose "resolve Pawn Shop's threat response" (isAbilityOf sid)
      pass
      cardPlayAreaOwner (UpgradeRef $ upgradeIdOf inspired) `shouldReturn` Just first
      for_ main $ \sid -> getMainSchemeCode sid `shouldReturn` Just "60137b"
      for_ main $ \sid -> getSchemeThreatAmount (SchemeMainSchemeId sid) `shouldReturn` 1
      next <- putInHand second "01074"
      offers (isPlay next) `shouldReturn` False

  for_ ["60121a","60128a","60134a","60142a","60151a","60161a"] $ \code ->
    for_ [Normal,Expert] $ \mode ->
      it ("completes setup for " <> show code <> " on " <> show mode) do
        tg <- scenarioGame code mode ["spider-man"]
        inGame tg do
          selectListCount AnyVillain `shouldReturn` 1
          selectListCount EncounterMainScheme `shouldReturn` 1

  it "Protection Racket gives each player their own physical business and routes minion schemes to its owner" do
    tg <- scenarioGame "60134a" Normal ["spider-man","she-hulk"]
    inGame tg do
      first <- getOwnedMainScheme (player1 tg)
      second <- getOwnedMainScheme (player2 tg)
      first `shouldSatisfy` isJust
      second `shouldSatisfy` isJust
      first `shouldSatisfy` (/= second)
      mid <- addMinion (player1 tg) Cards.street60130
      resolve [MinionMessage mid MinionSchemes]
      for_ first $ \scheme -> getSchemeThreatAmount (SchemeMainSchemeId scheme) `shouldReturn` 2
      for_ second $ \scheme -> getSchemeThreatAmount (SchemeMainSchemeId scheme) `shouldReturn` 0

  it "Protection Racket applies a villain's scheming to every business outside the player areas" do
    tg <- scenarioGame "60134a" Normal ["spider-man","she-hulk"]
    inGame tg do
      vid <- villainIdNow
      _ <- setEncounterDeck [Cards.street60188,Cards.street60188] []
      resolve [VillainMessage vid $ VillainSchemesAgainst (player1 tg)]
      schemes <- selectList EncounterMainScheme
      traverse getSchemeThreatAmount schemes `shouldReturn` [1,1]

  it "The Getaway starts Out Front with a speed counter and redirects villain damage into threat removal" do
    tg <- scenarioGame "60128a" Normal ["spider-man"]
    inGame tg do
      getCountersOnMainSchemes `shouldReturn` 1
      mainSchemeThreatNow `shouldReturn` 2
      selectAny (AttachmentIs Cards.street60129a) `shouldReturn` True
      vid <- villainIdNow
      before <- getVillainHitPoints vid
      resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 1 FromAbility]
      mainSchemeThreatNow `shouldReturn` 1
      getVillainHitPoints vid `shouldReturn` before

  it "The Raft starts with its physical Master Key and a Prisoner engaged with each player" do
    tg <- scenarioGame "60142a" Normal ["spider-man","she-hulk"]
    inGame tg do
      selectListCount (AttachmentIs Cards.street60143) `shouldReturn` 1
      for_ [player1 tg,player2 tg] $ \you ->
        selectListCount (MinionWithTrait T.Prisoner <> MinionEngagedWith (IdentityWithId you)) `shouldReturn` 1

  it "The Raft removes threat when an ordinary minion is defeated" do
    tg <- scenarioGame "60142a" Normal ["spider-man"]
    inGame tg do
      before <- mainSchemeThreatNow
      mid <- addMinion (player1 tg) Cards.street60130
      resolve [MinionMessage mid $ MinionDamaged GameRef $ withUnpreventable $ toDamage 10 FromAbility]
      mainSchemeThreatNow `shouldReturn` subtractNatural 1 before

  it "Stop the Presses deals each player a staff support with three uses" do
    tg <- scenarioGame "60151a" Normal ["spider-man","she-hulk"]
    inGame tg do
      for_ [player1 tg,player2 tg] $ \you -> do
        supports <- selectList $ SupportWithTrait T.DailyBugle <> SupportControlledBy (IdentityWithId you)
        length supports `shouldBe` 1
        traverse getCountersOn supports `shouldReturn` [3]

  it "Stop the Presses loses immediately when a Daily Bugle support leaves play" do
    tg <- scenarioGame "60151a" Normal ["spider-man"]
    inGame tg do
      staff <- selectJust $ SupportWithTrait T.DailyBugle
      resolve =<< L.runQueueT (L.capture $ L.removeFromPlay staff)
      getsGame gameState `shouldReturn` Finished Lost

  it "Kingpin begins behind the scenes with Public Support and a nemesis minion" do
    tg <- scenarioGame "60161a" Normal ["spider-man"]
    inGame tg do
      selectAny (VillainWithCardCode "60159a") `shouldReturn` True
      selectAny (EnvironmentIs Cards.street60163a) `shouldReturn` True
      selectListCount AnyMinion `shouldReturn` 1
      vid <- villainIdNow
      before <- getVillainHitPoints vid
      resolve [VillainMessage vid $ VillainDamaged GameRef $ withUnpreventable $ toDamage 10 FromAbility]
      getVillainHitPoints vid `shouldReturn` before
