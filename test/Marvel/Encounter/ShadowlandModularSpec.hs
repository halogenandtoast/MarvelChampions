module Marvel.Encounter.ShadowlandModularSpec (spec) where

import Marvel.FearNoEvil.Cards qualified as FNE
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Query
import Marvel.Shadowland.Cards qualified as Cards
import Marvel.Trait qualified as Trait
import TestImport

corruptionFor :: IdentityId -> GameAppT UpgradeId
corruptionFor you = do
  card <- encounterCard Cards.street65072a
  resolve [PutEncounterCardIntoPlay you card]
  selectJust $ UpgradeControlledBy (IdentityWithId you) <> UpgradeNamed "Creeping Corruption"

spec :: Spec
spec = describe "Shadowland modular card behaviors" do
  for_ [Cards.street65030,Cards.street65031,Cards.street65032,Cards.street65033,Cards.street65034] $ \def ->
    it ("defeating " <> show (toCardCode def) <> " discards two cards and reveals a Teamwork minion among them") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        mid <- addMinion you def
        when (toCardCode def == "65030") $ resolve [MinionMessage mid $ MinionDamaged GameRef $ toDamage 1 FromAbility]
        ([discarded,replacement,_,_],_) <- setEncounterDeck [FNE.street60085,Cards.street65030,FNE.street60188,FNE.street60188] []
        resolve [MinionMessage mid $ MinionDamaged (IdentityRef you) $ toDamage 2 FromAbility]
        selectAny (MinionWithId mid) `shouldReturn` False
        selectAny (MinionWithId $ MinionId $ unCardId $ ecCardId replacement) `shouldReturn` True
        selectListCount AnyMinion `shouldReturn` 1
        getEncounterDiscard >>= \cards -> liftIO $ map ecCardId cards `shouldContain` [ecCardId discarded]

  it "Kirigi shuffles his physical card into the encounter deck when the defeating player cannot pay" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      clearHand you
      mid <- addMinion you Cards.street65029
      _ <- setEncounterDeck (replicate 3 FNE.street60188) []
      resolve [MinionMessage mid $ MinionDamaged (IdentityRef you) $ toDamage 3 FromAbility]
      selectAny (MinionWithId mid) `shouldReturn` False
      getEncounterDeck >>= \cards -> liftIO $ map ecCardId cards `shouldContain` [CardId $ unMinionId mid]
      getEncounterDiscard >>= \cards -> liftIO $ map ecCardId cards `shouldNotContain` [CardId $ unMinionId mid]

  it "Kirigi stays discarded when the defeating player spends two resources" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      clearHand you
      energy <- putInHand you "01088"
      mid <- addMinion you Cards.street65029
      _ <- setEncounterDeck (replicate 3 FNE.street60188) []
      resolve [MinionMessage mid $ MinionDamaged (IdentityRef you) $ toDamage 3 FromAbility]
      discardOf you >>= \cards -> liftIO $ map pcCardId cards `shouldContain` [pcCardId energy]
      getEncounterDiscard >>= \cards -> liftIO $ map ecCardId cards `shouldContain` [CardId $ unMinionId mid]

  it "Aka's interrupt pays corruption to cancel his attack before boosts or damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      uid <- corruptionFor you
      mid <- addMinion you Cards.street65073
      changeForm you
      clearHand you
      (deck,_) <- setEncounterDeck (replicate 3 FNE.street60188) []
      runNow [MinionMessage mid $ MinionAttacks you]
      choose "accept corruption to cancel Aka" (isAbilityOf mid)
      pass
      getCountersOn uid `shouldReturn` 1
      damageOf you `shouldReturn` 0
      getEncounterDeck `shouldReturn` deck

  it "Tongue of the Beast corrupts the defeating player's identity" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      own <- corruptionFor (player1 tg)
      other <- corruptionFor (player2 tg)
      mid <- addMinion (player2 tg) Cards.street65074
      resolve [MinionMessage mid $ MinionDamaged (IdentityRef $ player1 tg) $ toDamage 1 $ FromPlayerAttack $ player1 tg]
      getCountersOn own `shouldReturn` 2
      getCountersOn other `shouldReturn` 0

  it "Binding Ritual resolves each player's corruption-or-threat decision independently" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      own <- corruptionFor (player1 tg)
      other <- corruptionFor (player2 tg)
      _ <- revealNow (player1 tg) Cards.street65075
      choose "accept corruption for the first player" (isLabel "$label.slCorrupt")
      choose "add threat for the second player" (isLabel "$label.slResist")
      pass
      sid <- selectJust $ SideSchemeIs Cards.street65075
      getSchemeThreatAmount (SchemeSideSchemeId sid) `shouldReturn` 5
      getCountersOn own `shouldReturn` 1
      getCountersOn other `shouldReturn` 0

  it "defeating Roots of a Black Flower places two corruption counters on the thwarting player's upgrade" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      uid <- corruptionFor you
      card <- revealNow you Cards.street65076
      pass
      let sid = SideSchemeId $ unCardId $ ecCardId card
      resolve =<< L.runQueueT (L.capture $ L.removeSchemeThreat you (SchemeSideSchemeId sid) 1)
      getCountersOn uid `shouldReturn` 2

  it "Malediction's alter-ego corruption choice avoids its enhanced scheme" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      uid <- corruptionFor you
      before <- mainSchemeThreatNow
      _ <- revealNow you Cards.street65077
      choose "take two corruption counters" (isLabel "$label.slCorrupt")
      pass
      getCountersOn uid `shouldReturn` 2
      mainSchemeThreatNow `shouldReturn` before

  it "Invasive Thoughts checks Overpowering after the newly placed counter can flip the upgrade" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      uid <- corruptionFor you
      card <- encounterCard Cards.street65078
      resolve [RevealEncounterCard you card]
      damageOf you `shouldReturn` 1
      getCountersOn uid `shouldReturn` 1
      resolve [PlaceCounters (UpgradeRef uid) 3]
      second <- encounterCard Cards.street65078
      before <- mainSchemeThreatNow
      resolve [RevealEncounterCard you second]
      selectAny (UpgradeWithId uid <> UpgradeWithTrait Trait.Overpowering) `shouldReturn` True
      mainSchemeThreatNow `shouldReturn` (before + 1)
      damageOf you `shouldReturn` 1
