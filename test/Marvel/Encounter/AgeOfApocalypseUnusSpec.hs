module Marvel.Encounter.AgeOfApocalypseUnusSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Keyword hiding (Surge)
import Marvel.Keyword qualified as Keyword
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Phase
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.SideScheme.SideSchemes.AgeOfApocalypseInfinites (genePoolThreat, placeGenePoolThreat)
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import TestImport

unusGame :: Difficulty -> IO TestGame
unusGame mode = do
  let setup = (fromMaybe (error "Unus setup missing") $ recommendedSetup "45059") {difficulty = mode}
  (application, trace, logs) <- dealtGameLogged setup [starter "spider-man"] 1
  seating <- runGameApp application (getsGame gameSeats)
  let tg = TestGame application trace logs seating
  inGame tg toFirstTurn
  pure tg

lifted :: L.QueueT GameAppT () -> GameAppT ()
lifted body = resolve =<< L.runQueueT (L.capture body)

setGenePool :: Natural -> GameAppT ()
setGenePool n = do
  current <- genePoolThreat
  sid <- selectJust (SideSchemeIs Schemes.genePool)
  if current > n then resolve [SideSchemeMessage sid (SideSchemeThreatRemoved GameRef (current - n))]
    else lifted $ placeGenePoolThreat (n - current)

minionAttrs :: MinionId -> GameAppT (Attrs Minion.Minion)
minionAttrs mid = getsGame (toAttrs . fromMaybe (error "missing minion") . HashMap.lookup mid . gameMinions)

attachmentOf :: EncounterCard -> AttachmentId
attachmentOf = AttachmentId . unCardId . ecCardId

attachmentHost :: EncounterCard -> GameAppT (Maybe EnemyId)
attachmentHost card = getsGame (join . fmap (Attachment.attachmentEnemy . toAttrs) . HashMap.lookup (attachmentOf card) . gameAttachments)

attachmentExists :: EncounterCard -> GameAppT Bool
attachmentExists card = getsGame (HashMap.member (attachmentOf card) . gameAttachments)

boostNow :: CardDef -> GameAppT ()
boostNow def = do
  card <- encounterCard def
  vid <- villainIdNow
  resolve [RevealBoostCard card (EnemyVillainId vid)]

answerUntil :: (Choice -> Bool) -> GameAppT Bool
answerUntil predicate = go (30 :: Int)
 where
  go 0 = pure False
  go n = offers predicate >>= \case
    True -> pure True
    False -> offers isNoResponse >>= \case
      True -> choose "decline response" isNoResponse >> go (n - 1)
      False -> pure False

spec :: Spec
spec = describe "Age of Apocalypse Unus" do
  for_ [(Normal,"45059",0),(Expert,"45060",1)] $ \(mode, code, facedown) ->
    it ("sets up printed Gene Pool, correct villain, and expert encounter cards: " <> show mode) do
      tg <- unusGame mode
      inGame tg do
        genePoolThreat `shouldReturn` 4
        villain <- getVillain
        toCardCode (getCardDef villain) `shouldBe` code
        Villain.villainTough (toAttrs villain) `shouldBe` True
        length <$> dealtTo (player1 tg) >>= (`shouldBe` facedown)
        deck <- getEncounterDeck
        map toCardCode deck `shouldSatisfy` notElem "45071"

  it "offers optional additional Gene Pool threat without changing the default" do
    tg <- unusGame Normal
    inGame tg do
      sid <- mainSchemeIdNow
      runNow [SetupMainScheme]
      offers (isLabel "$label.aoaGenePoolNoExtra") `shouldReturn` True
      choose "optional extra threat" (isLabel "$label.aoaGenePoolExtraTwo")
      genePoolThreat `shouldReturn` 6
      main <- mainSchemeIdNow
      main `shouldBe` sid

  for_ ["45059","45060","45061"] $ \code ->
    it ("Unus gains and loses cumulative Gene Pool keywords on stage " <> show code) do
      tg <- unusGame Normal
      inGame tg do
        vid <- villainIdNow
        resolve [ReplaceVillain vid code Swapped]
        for_ [(2,False,False,False),(3,True,False,False),(6,True,True,False),(9,True,True,True),(0,False,False,False)] $ \(threat, retaliateOn, stalwartOn, amplifyOn) -> do
          setGenePool threat
          a <- toAttrs <$> getVillain
          keys <- Villain.getModifiedKeywords a
          (Retaliate 1 `elem` keys) `shouldBe` retaliateOn
          (Stalwart `elem` keys) `shouldBe` stalwartOn
          mods <- getModifiers (VillainRef vid)
          (AmplifyIconModifier 1 `elem` mods) `shouldBe` amplifyOn

  it "places one Gene Pool threat after villain-phase step one" do
    tg <- unusGame Normal
    inGame tg do
      resolve [VillainPhaseStep StepEnemiesActivate]
      genePoolThreat `shouldReturn` 5
      resolve [VillainPhaseStep StepDealEncounterCards]
      genePoolThreat `shouldReturn` 5

  it "loses when Hunting Gene Traitors completes" do
    tg <- unusGame Normal
    inGame tg do
      sid <- mainSchemeIdNow
      lifted $ L.placeThreat sid 11
      getsGame gameState `shouldReturn` Finished Lost

  it "Infinite Soldier gains and loses quickstrike, surge, and three hit points" do
    tg <- unusGame Normal
    inGame tg do
      setGenePool 0
      mid <- addMinion (player1 tg) Minions.infiniteSoldier
      for_ [(2,False,False,3),(3,True,False,3),(6,True,True,3),(9,True,True,6),(0,False,False,3)] $ \(threat, quick, surges, hp) -> do
        setGenePool threat
        a <- minionAttrs mid
        keys <- Minion.getModifiedKeywords a
        (Guard `elem` keys) `shouldBe` True
        (Quickstrike `elem` keys) `shouldBe` quick
        (Keyword.Surge `elem` keys) `shouldBe` surges
        Minion.getModifiedHitPoints a `shouldReturn` hp

  it "Infinite Soldier quickstrikes when Gene Pool has at least three threat" do
    tg <- unusGame Normal
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      before <- damageOf you
      _ <- addMinion you Minions.infiniteSoldier
      damageOf you `shouldReturn` before + 2

  it "Gene Pool adds three threat for an ally defeated by ability damage" do
    tg <- unusGame Normal
    inGame tg do
      ally <- putIntoPlay (player1 tg) "01002"
      lifted $ L.damageCharacter (allyIdOf ally) GameRef (toDamage 2 FromAbility)
      genePoolThreat `shouldReturn` 7

  it "Gene Pool ignores consequential defeat and ordinary discards" do
    tg <- unusGame Normal
    inGame tg do
      ally <- putIntoPlay (player1 tg) "01002"
      resolve [AllyMessage (allyIdOf ally) (AllySetDamage 1), AllyMessage (allyIdOf ally) AllyThwartConsequential]
      genePoolThreat `shouldReturn` 4
      discarded <- putIntoPlay (player1 tg) "01002"
      resolve [RemoveFromPlay (AllyRef $ allyIdOf discarded)]
      genePoolThreat `shouldReturn` 4

  it "Gene Pool remains in play when all its threat is removed" do
    tg <- unusGame Normal
    inGame tg do
      setGenePool 0
      selectAny (SideSchemeIs Schemes.genePool) `shouldReturn` True

  it "Culling the Weak places four threat revealed and two as a boost" do
    tg <- unusGame Normal
    inGame tg do
      _ <- revealNow (player1 tg) Treacheries.cullingTheWeak
      resolve []
      genePoolThreat `shouldReturn` 8
      boostNow Treacheries.cullingTheWeak
      genePoolThreat `shouldReturn` 10

  for_ [(Attachments.prelateSidearm,1,0),(Attachments.prelateArmor,0,1)] $ \(def, atk, sch) ->
    it ("Prelate equipment attaches to Unus and changes its printed stat: " <> show (toCardCode def)) do
      tg <- unusGame Normal
      inGame tg do
        vid <- villainIdNow
        card <- revealNow (player1 tg) def
        resolve []
        attachmentHost card `shouldReturn` Just (EnemyVillainId vid)
        getVillainAttackValue vid `shouldReturn` 2 + atk
        getVillainSchemeValue vid `shouldReturn` 1 + sch

  it "Prelate Armor gives Unus tough after he schemes" do
    tg <- unusGame Normal
    inGame tg do
      let you = player1 tg
      _ <- revealNow you Attachments.prelateArmor
      resolve []
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainDamaged GameRef (toDamage 1 FromAbility))]
      Villain.villainTough . toAttrs <$> getVillain `shouldReturn` False
      _ <- setEncounterDeck (replicate 5 Treacheries.hardToKeepDown) []
      resolve [VillainMessage vid (VillainSchemesAgainst you)]
      Villain.villainTough . toAttrs <$> getVillain `shouldReturn` True

  it "Prelate Sidearm places one extra Gene Pool threat after Unus defeats an ally" do
    tg <- unusGame Normal
    inGame tg do
      let you = player1 tg
      _ <- revealNow you Attachments.prelateSidearm
      resolve []
      ally <- putIntoPlay you "01002"
      vid <- villainIdNow
      _ <- setEncounterDeck (replicate 5 Treacheries.hardToKeepDown) []
      resolve [VillainMessage vid (VillainAttacksAlly you $ allyIdOf ally)]
      -- Three from the ally defeat and one from the Sidearm.
      genePoolThreat `shouldReturn` 8

  for_ [(Attachments.prelateSidearm,"01088"),(Attachments.prelateArmor,"01089")] $ \(def, code) ->
    it ("a basic attack offers the resource-paid discard response: " <> show (toCardCode def)) do
      tg <- unusGame Normal
      inGame tg do
        let you = player1 tg
        changeForm you
        clearHand you
        resource <- putInHand you code
        physical <- putInHand you "01090"
        card <- revealNow you def
        resolve []
        choose "basic attack" (isHeroAttack you)
        chooseVillain
        answerUntil (isAbilityOf $ AttachmentRef $ attachmentOf card) `shouldReturn` True
        choose "discard equipment" (isAbilityOf $ AttachmentRef $ attachmentOf card)
        payWith [resource,physical]
        resolve []
        attachmentExists card `shouldReturn` False

  it "Infinite Hunter deals three damage to an ally when revealed" do
    tg <- unusGame Normal
    inGame tg do
      let you = player1 tg
      ally <- putIntoPlay you "01083"
      _ <- addMinion you Minions.infiniteHunter
      getsGame (HashMap.member (allyIdOf ally) . gameAllies) `shouldReturn` False
      genePoolThreat `shouldReturn` 7

  it "Infinite Hunter does not damage the identity if there is no ally" do
    tg <- unusGame Normal
    inGame tg do
      _ <- addMinion (player1 tg) Minions.infiniteHunter
      damageOf (player1 tg) `shouldReturn` 0

  it "Infinite Hunter boost offers threat or activation bonus" do
    tg <- unusGame Normal
    inGame tg do
      card <- encounterCard Minions.infiniteHunter
      vid <- villainIdNow
      runNow [RevealBoostCard card (EnemyVillainId vid)]
      offers (isLabel "$label.aoaActivationBonusTwo") `shouldReturn` True
      choose "place threat" (isLabel "$label.aoaPlaceTwoGenePoolThreat")
      resolve []
      genePoolThreat `shouldReturn` 6

  it "Genetic Experiments attaches only to an Infinite and increases each stat" do
    tg <- unusGame Normal
    inGame tg do
      let you = player1 tg
      _ <- addMinion you Minions.hydraMercenary
      mid <- addMinion you Minions.infiniteSoldier
      card <- revealNow you Attachments.geneticExperiments
      resolve []
      attachmentHost card `shouldReturn` Just (EnemyMinionId mid)
      a <- minionAttrs mid
      Minion.getModifiedAttack a `shouldReturn` 3
      Minion.getModifiedScheme a `shouldReturn` 2
      Minion.getModifiedHitPoints a `shouldReturn` 5
      lifted $ L.dealDamage mid GameRef (toDamage 5 FromAbility)
      genePoolThreat `shouldReturn` 6

  it "Genetic Experiments boost attaches the physical boost to an Infinite" do
    tg <- unusGame Normal
    inGame tg do
      mid <- addMinion (player1 tg) Minions.infiniteSoldier
      card <- encounterCard Attachments.geneticExperiments
      vid <- villainIdNow
      resolve [RevealBoostCard card (EnemyVillainId vid)]
      attachmentHost card `shouldReturn` Just (EnemyMinionId mid)
      a <- minionAttrs mid
      Minion.getModifiedHitPoints a `shouldReturn` 5

  it "Genetic Experiments surges without an Infinite minion" do
    tg <- unusGame Normal
    inGame tg do
      let you = player1 tg
      _ <- addMinion you Minions.hydraMercenary
      before <- length <$> dealtTo you
      _ <- revealNow you Attachments.geneticExperiments
      resolve []
      length <$> dealtTo you >>= (`shouldBe` before + 1)

  it "Infinite Prelate activates Unus then grants its lower-tier bonuses" do
    tg <- unusGame Normal
    inGame tg do
      setGenePool 6
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainDamaged GameRef (toDamage 1 FromAbility)), VillainMessage vid (VillainDamaged GameRef (toDamage 4 FromAbility))]
      _ <- setEncounterDeck (replicate 5 Treacheries.hardToKeepDown) []
      before <- mainSchemeThreat
      _ <- revealNow (player1 tg) Treacheries.infinitePrelate
      resolve []
      mainSchemeThreat `shouldReturn` before + 1
      villainDamageNow `shouldReturn` 1
      Villain.villainTough . toAttrs <$> getVillain `shouldReturn` True

  it "Endless Ranks has four fixed threat and adds three to Gene Pool when defeated" do
    tg <- unusGame Normal
    inGame tg do
      card <- revealNow (player1 tg) Schemes.endlessRanks
      resolve []
      sideSchemeThreat (sideSchemeOf card) `shouldReturn` 4
      resolve [SideSchemeMessage (sideSchemeOf card) (SideSchemeThwarted (toSource $ player1 tg) 4)]
      genePoolThreat `shouldReturn` 7

  it "Hunted remains until its owner discards a card in alter ego" do
    tg <- unusGame Normal
    inGame tg do
      let you = player1 tg
      card <- revealNow you Obligations.hunted
      resolve []
      held <- putInHand you "01088"
      let oid = ObligationId $ unCardId $ ecCardId card
      choose "discard Hunted" (isAbilityOf $ ObligationRef oid)
      choose "discard resource" (isTargetChoice held)
      resolve []
      getsGame (HashMap.member oid . gameObligations) `shouldReturn` False

  it "War-Weary stuns, then deals two damage instead when already stunned" do
    tg <- unusGame Normal
    inGame tg do
      let you = player1 tg
      _ <- revealNow you Treacheries.warWearyDystopianNightmare
      resolve []
      isStunned you `shouldReturn` True
      damageOf you `shouldReturn` 0
      _ <- revealNow you Treacheries.warWearyDystopianNightmare
      resolve []
      damageOf you `shouldReturn` 2
      isStunned you `shouldReturn` True

  it "War-Weary has the same stun-or-damage boost ability" do
    tg <- unusGame Normal
    inGame tg do
      boostNow Treacheries.warWearyDystopianNightmare
      isStunned (player1 tg) `shouldReturn` True
      boostNow Treacheries.warWearyDystopianNightmare
      damageOf (player1 tg) `shouldReturn` 2

  it "Targeted for Extermination confuses the player whose thwart defeated it" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let you = player2 tg
      card <- revealNow (player1 tg) Schemes.targetedForExtermination
      resolve []
      resolve [SideSchemeMessage (sideSchemeOf card) (SideSchemeThwarted (toSource you) 3)]
      isConfused you `shouldReturn` True
      isConfused (player1 tg) `shouldReturn` False

  it "Infinite Hunter activation bonus raises the current scheme by two" do
    tg <- unusGame Normal
    inGame tg do
      setGenePool 0
      vid <- villainIdNow
      _ <- setEncounterDeck [Minions.infiniteHunter,Treacheries.hardToKeepDown] []
      before <- mainSchemeThreat
      runNow [VillainMessage vid (VillainSchemesAgainst $ player1 tg)]
      choose "activation bonus" (isLabel "$label.aoaActivationBonusTwo")
      resolve []
      mainSchemeThreat `shouldReturn` before + 3
      genePoolThreat `shouldReturn` 0

  it "Infinite Prelate gives its activation an extra boost at nine threat" do
    tg <- unusGame Normal
    inGame tg do
      setGenePool 9
      -- Two two-icon cards become three-icon cards because Unus has amplify.
      _ <- setEncounterDeck (replicate 5 Treacheries.explosion) []
      before <- mainSchemeThreat
      _ <- revealNow (player1 tg) Treacheries.infinitePrelate
      resolve []
      mainSchemeThreat `shouldReturn` before + 7

  it "Infinite Prelate grants no tough or healing below three threat" do
    tg <- unusGame Normal
    inGame tg do
      setGenePool 0
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainDamaged GameRef (toDamage 1 FromAbility)), VillainMessage vid (VillainDamaged GameRef (toDamage 4 FromAbility))]
      _ <- setEncounterDeck (replicate 5 Treacheries.hardToKeepDown) []
      _ <- revealNow (player1 tg) Treacheries.infinitePrelate
      resolve []
      Villain.villainTough . toAttrs <$> getVillain `shouldReturn` False
      villainDamageNow `shouldReturn` 4

  it "Genetic Experiments boost has no effect and does not surge without an Infinite" do
    tg <- unusGame Normal
    inGame tg do
      vid <- villainIdNow
      card <- encounterCard Attachments.geneticExperiments
      resolve [RevealBoostCard card (EnemyVillainId vid),ClearBoosts]
      getsGame (HashMap.member (attachmentOf card) . gameAttachments) `shouldReturn` False
      length <$> dealtTo (player1 tg) >>= (`shouldBe` 0)
