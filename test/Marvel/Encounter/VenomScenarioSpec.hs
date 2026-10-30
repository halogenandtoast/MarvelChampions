module Marvel.Encounter.VenomScenarioSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Environment.Environments.SmVenom (bellTowers)
import Marvel.Environment.Types qualified as Environment
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import Marvel.Keyword (Keyword (Overkill))
import Marvel.Villain.Types qualified as Villain
import Marvel.Window qualified as W
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Status (StatusCard (..))
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

settle :: GameAppT ()
settle = go (150 :: Int)
 where
  go 0 = error "Venom did not settle"
  go n = openQuestion >>= \case
    Just (_, QuestionLabel "$label.yourTurn" _ _) -> pure ()
    Just (ident, q) -> do
      let passing c = isNoResponse c || isNoDefender c || isLabel "$label.declineToDefend" c
          index = fromMaybe 0 $ headMay [i | (i, c) <- zip [0 ..] (choicesOf q), passing c]
      answerQuestion ident index
      go (n - 1)
    Nothing -> pure ()

tower :: GameAppT EnvironmentId
tower = fromMaybe (error "Bell Tower missing") . headMay <$> bellTowers

towerNow :: GameAppT (CardCode, Natural)
towerNow = do
  eid <- tower
  getsGame $ \game -> case HashMap.lookup eid (gameEnvironments game) of
    Just e -> (toCardCode $ getCardDef e, Environment.environmentCounters $ toAttrs e)
    Nothing -> error "Bell Tower missing"

identityBoosts :: IdentityId -> GameAppT Int
identityBoosts you = length . (.playerIdentityBoostCards) <$> playerAttrs you

attackMessages :: IdentityId -> Natural -> GameAppT [Message]
attackMessages you n = do
  vid <- villainIdNow
  messages <- L.runQueueT (L.capture $ L.dealDamage vid you $ toDamage n $ FromPlayerAttack you)
  pure $ messages <> [CheckWindows [W.Window W.After $ W.PlayerAttackResolved (toSource you) you [EnemyVillainId vid]]]

attackWithoutChimes :: IdentityId -> Natural -> GameAppT ()
attackWithoutChimes you n = attackMessages you n >>= runNow >> settle

clearVenomTough :: GameAppT ()
clearVenomTough = villainIdNow >>= \vid -> resolve [RemoveStatus (VillainRef vid) ToughStatus]

spec :: Spec
spec = describe "Venom scenario" do
  it "sets up the Quiet Bell Tower and Venom's tough status" do
    tg <- testGameAgainst "27073" ["spider-man"]
    inGame tg do
      towerNow `shouldReturn` ("27077a", 0)
      vid <- villainIdNow
      -- Toughness is printed; the status was placed by ordinary villain setup.
      tough <- getsGame (maybe False (Villain.villainTough . toAttrs) . HashMap.lookup vid . gameVillains)
      liftIO $ tough `shouldBe` True

  it "gives a facedown identity boost after an identity attack damages Venom" do
    tg <- testGameAgainst "27073" ["spider-man"]
    inGame tg do
      clearVenomTough
      _ <- setEncounterDeck (replicate 8 Treacheries.imTough) []
      attackWithoutChimes (player1 tg) 2
      villainDamageNow `shouldReturn` 2
      identityBoosts (player1 tg) `shouldReturn` 1

  it "does not give identity boost cards for ability damage or an attack stopped by tough" do
    tg <- testGameAgainst "27073" ["spider-man"]
    inGame tg do
      attackWithoutChimes (player1 tg) 2
      identityBoosts (player1 tg) `shouldReturn` 0
      vid <- villainIdNow
      resolve =<< L.runQueueT (L.capture $ L.dealDamage vid (player1 tg) $ toDamage 2 FromAbility)
      villainDamageNow `shouldReturn` 2
      identityBoosts (player1 tg) `shouldReturn` 0

  it "puts an ally attack's identity boost on that ally's controller" do
    tg <- testGameAgainst "27073" ["spider-man", "captain-marvel"]
    inGame tg do
      clearVenomTough
      cat <- putIntoPlay (player2 tg) "01002"
      vid <- villainIdNow
      runNow =<< L.runQueueT (L.capture $ L.dealDamage vid (allyIdOf cat) $ toDamage 1 $ FromAllyAttack $ allyIdOf cat)
      settle
      -- Ally attacks emit their completion window through the ally runner.
      runNow [CheckWindows [W.Window W.After $ W.PlayerAttackResolved (toSource $ allyIdOf cat) (player2 tg) [EnemyVillainId vid]]]
      settle
      identityBoosts (player1 tg) `shouldReturn` 0
      identityBoosts (player2 tg) `shouldReturn` 1

  it "moves identity boost cards to Venom when he schemes and resolves them with his normal boost" do
    tg <- testGameAgainst "27073" ["spider-man"]
    inGame tg do
      _ <- setEncounterDeck (replicate 8 Treacheries.imTough) []
      resolve =<< L.runQueueT (L.capture $ L.giveIdentityBoostCards (player1 tg) 2)
      identityBoosts (player1 tg) `shouldReturn` 2
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainSchemesAgainst $ player1 tg)]
      identityBoosts (player1 tg) `shouldReturn` 0
      mainSchemeThreatNow `shouldReturn` 3

  it "leaves identity boost cards in place when confusion replaces Venom's activation" do
    tg <- testGameAgainst "27073" ["spider-man"]
    inGame tg do
      _ <- setEncounterDeck (replicate 8 Treacheries.imTough) []
      resolve =<< L.runQueueT (L.capture $ L.giveIdentityBoostCards (player1 tg) 1)
      vid <- villainIdNow
      resolve [GiveStatus (VillainRef vid) ConfusedStatus, VillainMessage vid (VillainSchemesAgainst $ player1 tg)]
      identityBoosts (player1 tg) `shouldReturn` 1

  it "Quiet converts attack damage to counters without damaging Venom or triggering Vengeance" do
    tg <- testGameAgainst "27073" ["spider-man"]
    inGame tg do
      clearVenomTough
      runNow =<< attackMessages (player1 tg) 2
      eid <- tower
      choose "convert the attack to chimes" (isAbilityOf eid)
      settle
      towerNow `shouldReturn` ("27077a", 2)
      villainDamageNow `shouldReturn` 0
      identityBoosts (player1 tg) `shouldReturn` 0

  it "Quiet flips at three chimes per starting player and retains every converted counter" do
    tg <- testGameAgainst "27073" ["spider-man", "captain-marvel"]
    inGame tg do
      eid <- tower
      resolve [PlaceCounters (EnvironmentRef eid) 5]
      towerNow `shouldReturn` ("27077a", 5)
      resolve [PlaceCounters (EnvironmentRef eid) 3]
      towerNow `shouldReturn` ("27077b", 8)

  it "Ringing increases ability damage as well as attack damage by one" do
    tg <- testGameAgainst "27073" ["spider-man"]
    inGame tg do
      clearVenomTough
      eid <- tower
      resolve [PlaceCounters (EnvironmentRef eid) 3]
      vid <- villainIdNow
      resolve =<< L.runQueueT (L.capture $ L.dealDamage vid (player1 tg) $ toDamage 1 FromAbility)
      villainDamageNow `shouldReturn` 2
      attackWithoutChimes (player1 tg) 1
      villainDamageNow `shouldReturn` 4

  it "Ringing spends at most its counters to prevent Venom's identity attack damage and flips at zero" do
    tg <- testGameAgainst "27073" ["spider-man"]
    inGame tg do
      changeForm (player1 tg)
      eid <- tower
      resolve [PlaceCounters (EnvironmentRef eid) 3]
      _ <- setEncounterDeck (replicate 8 Treacheries.imTough) []
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks $ player1 tg)]
      settle
      towerNow `shouldReturn` ("27077b", 1)
      damageOf (player1 tg) `shouldReturn` 0
      runNow [VillainMessage vid (VillainAttacks $ player1 tg)]
      settle
      towerNow `shouldReturn` ("27077a", 0)
      damageOf (player1 tg) `shouldReturn` 1

  it "Guard the Bell Tower resets Quiet and blanks its counter thresholds and damage bonus" do
    tg <- testGameAgainst "27073" ["spider-man"]
    inGame tg do
      clearVenomTough
      eid <- tower
      resolve [PlaceCounters (EnvironmentRef eid) 4]
      guard <- revealNow (player1 tg) SideSchemes.smGuardTheBellTower
      towerNow `shouldReturn` ("27077a", 0)
      resolve [PlaceCounters (EnvironmentRef eid) 4]
      towerNow `shouldReturn` ("27077a", 4)
      attackWithoutChimes (player1 tg) 2
      villainDamageNow `shouldReturn` 2
      towerNow `shouldReturn` ("27077a", 4)
      resolve [SideSchemeMessage (sideSchemeOf guard) (SideSchemeThwarted (toSource $ player1 tg) 3)]
      towerNow `shouldReturn` ("27077b", 4)

  it "Now We're Angry adds ATK and overkill and expires after two damaging attacks" do
    tg <- testGameAgainst "27073" ["spider-man"]
    inGame tg do
      clearVenomTough
      _ <- revealNow (player1 tg) Attachments.smNowWeReAngry
      vid <- villainIdNow
      modifiers <- getModifiers (VillainRef vid)
      liftIO do
        modifiers `shouldSatisfy` elem (AttackModifier 1)
        modifiers `shouldSatisfy` elem (KeywordModifier Overkill)
      attackWithoutChimes (player1 tg) 1
      counts <- getsGame (map (Attachment.attachmentCounters . toAttrs) . HashMap.elems . gameAttachments)
      liftIO $ counts `shouldBe` [1]
      attackWithoutChimes (player1 tg) 1
      getsGame (length . gameAttachments) `shouldReturn` 0

  it "Lashing Out can remove threat equal to attack damage dealt to Venom" do
    tg <- testGameAgainst "27073" ["spider-man"]
    inGame tg do
      clearVenomTough
      _ <- revealNow (player1 tg) SideSchemes.smGuardTheBellTower
      card <- revealNow (player1 tg) SideSchemes.smLashingOut
      runNow =<< attackMessages (player1 tg) 2
      choose "remove Lashing Out threat" (isAbilityOf $ sideSchemeOf card)
      settle
      sideSchemeThreat (sideSchemeOf card) `shouldReturn` 7

  it "Tooth and Nail can remove threat equal to attack damage dealt to Venom" do
    tg <- testGameAgainst "27073" ["spider-man"]
    inGame tg do
      clearVenomTough
      _ <- revealNow (player1 tg) SideSchemes.smGuardTheBellTower
      card <- revealNow (player1 tg) SideSchemes.smToothAndNail
      runNow =<< attackMessages (player1 tg) 2
      choose "remove Tooth and Nail threat" (isAbilityOf $ sideSchemeOf card)
      settle
      sideSchemeThreat (sideSchemeOf card) `shouldReturn` 6

  it "Biting Retort adds one icon to each boost card of its activation only" do
    tg <- testGameAgainst "27073" ["spider-man"]
    inGame tg do
      _ <- setEncounterDeck (replicate 8 Treacheries.imTough) []
      _ <- revealNow (player1 tg) Treacheries.smBitingRetort
      settle
      mainSchemeThreatNow `shouldReturn` 4
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainSchemesAgainst $ player1 tg)]
      mainSchemeThreatNow `shouldReturn` 5

  it "For Whom the Bell Tolls checks the side after removing counters and flipping" do
    tg <- testGameAgainst "27073" ["spider-man"]
    inGame tg do
      eid <- tower
      resolve [PlaceCounters (EnvironmentRef eid) 3, RemoveCounters (EnvironmentRef eid) 1]
      towerNow `shouldReturn` ("27077b", 2)
      _ <- revealNow (player1 tg) Treacheries.smForWhomTheBellTolls
      settle
      towerNow `shouldReturn` ("27077a", 0)
      damageOf (player1 tg) `shouldReturn` 1
      mainSchemeThreatNow `shouldReturn` 2

  it "For Whom the Bell Tolls removes threat if counters remain and the tower keeps ringing" do
    tg <- testGameAgainst "27073" ["spider-man"]
    inGame tg do
      eid <- tower
      resolve [PlaceCounters (EnvironmentRef eid) 3]
      _ <- revealNow (player1 tg) Treacheries.smForWhomTheBellTolls
      settle
      towerNow `shouldReturn` ("27077b", 1)
      damageOf (player1 tg) `shouldReturn` 0
      mainSchemeThreatNow `shouldReturn` 1

  it "stage II puts Tooth and Nail into play when revealed" do
    tg <- testGameAgainst "27073" ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      resolve [VillainMessage vid VillainDefeated]
      toCardCode . getCardDef <$> getVillain `shouldReturn` "27074"
      codes <- map fst <$> getSideSchemesInPlay
      liftIO $ codes `shouldSatisfy` elem "27081"

  it "stage III puts two boost cards on every identity when revealed" do
    tg <- testGameAgainst "27073" ["spider-man", "captain-marvel"]
    inGame tg do
      _ <- setEncounterDeck (replicate 12 Treacheries.imTough) []
      vid <- villainIdNow
      resolve [ReplaceVillain vid "27075" Advanced]
      identityBoosts (player1 tg) `shouldReturn` 2
      identityBoosts (player2 tg) `shouldReturn` 2

  it "stage III gives two boosts for the first damaging attack and one for a later attack" do
    tg <- testGameAgainst "27073" ["spider-man"]
    inGame tg do
      _ <- setEncounterDeck (replicate 16 Treacheries.imTough) []
      vid <- villainIdNow
      resolve [ReplaceVillain vid "27075" Advanced]
      clearVenomTough
      before <- identityBoosts (player1 tg)
      attackWithoutChimes (player1 tg) 1
      identityBoosts (player1 tg) `shouldReturn` before + 2
      attackWithoutChimes (player1 tg) 1
      identityBoosts (player1 tg) `shouldReturn` before + 3

  it "stage III counts an earlier attack against a minion as the first attack of the turn" do
    tg <- testGameAgainst "27073" ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- setEncounterDeck (replicate 16 Treacheries.imTough) []
      vid <- villainIdNow
      resolve [ReplaceVillain vid "27075" Advanced]
      clearVenomTough
      before <- identityBoosts you
      minion <- addMinion you Minions.hydraBomber
      choose "basic attack minion" (isHeroAttack you)
      choose "target minion" (isTargetChoice minion)
      settle
      attackWithoutChimes you 1
      identityBoosts you `shouldReturn` before + 1

  it "stage III counts an attack stopped by tough before a later damaging attack" do
    tg <- testGameAgainst "27073" ["spider-man"]
    inGame tg do
      _ <- setEncounterDeck (replicate 16 Treacheries.imTough) []
      vid <- villainIdNow
      resolve [ReplaceVillain vid "27075" Advanced]
      resolve [GiveStatus (VillainRef vid) ToughStatus]
      before <- identityBoosts (player1 tg)
      attackWithoutChimes (player1 tg) 1
      identityBoosts (player1 tg) `shouldReturn` before
      attackWithoutChimes (player1 tg) 1
      identityBoosts (player1 tg) `shouldReturn` before + 1

  it "stage III counts an attack converted to chimes before a later damaging attack" do
    tg <- testGameAgainst "27073" ["spider-man"]
    inGame tg do
      _ <- setEncounterDeck (replicate 16 Treacheries.imTough) []
      vid <- villainIdNow
      resolve [ReplaceVillain vid "27075" Advanced]
      clearVenomTough
      before <- identityBoosts (player1 tg)
      runNow =<< attackMessages (player1 tg) 1
      eid <- tower
      choose "convert the first attack to chimes" (isAbilityOf eid)
      settle
      identityBoosts (player1 tg) `shouldReturn` before
      towerNow `shouldReturn` ("27077a", 1)
      attackWithoutChimes (player1 tg) 1
      identityBoosts (player1 tg) `shouldReturn` before + 1

  it "a labeled event hitting a minion and Venom is one attack for Retribution" do
    tg <- testGameAgainst "27073" ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- setEncounterDeck (replicate 16 Treacheries.imTough) []
      vid <- villainIdNow
      resolve [ReplaceVillain vid "27075" Advanced]
      clearVenomTough
      minion <- addMinion you Minions.hydraMercenary
      before <- identityBoosts you
      melee <- putInHand you "05030"
      energy <- putInHand you "01088"
      payment <- putInHand you "01008"
      playWith melee [energy, payment]
      choose "Melee's minion target" (isTargetChoice minion)
      choose "Melee's Venom target" (isTargetChoice vid)
      settle
      identityBoosts you `shouldReturn` before + 2
