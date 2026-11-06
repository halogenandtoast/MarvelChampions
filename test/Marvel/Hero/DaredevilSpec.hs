module Marvel.Hero.DaredevilSpec (spec) where

import Marvel.CampaignPlayers.Helpers qualified as Pack
import Marvel.CampaignPlayerTest
import Marvel.Minion.Cards qualified as Minions
import Marvel.Message.Lifted qualified as L
import Marvel.Status
import TestImport

spec :: Spec
spec = describe "Daredevil" do
  it "creates a five-card ordered Sense deck outside his ordinary deck" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      senses <- Pack.identitySenseDeck p
      map toCardCode senses `shouldMatchList` ["60002","60003","60004","60005","60006"]
      deck <- unDeck . playerIdentityDeck . toAttrs <$> getPlayer p
      map toCardCode deck `shouldSatisfy` all (`notElem` ["60002","60003","60004","60005","60006"])

  it "has the printed basic hero powers" do
    tg <- testGame ["daredevil"]
    inGame tg do
      asHero (player1 tg)
      heroPowers (player1 tg) `shouldReturn` (2,2,2)

  it "Cross-Examination deals three damage without upgrades" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- paying p "60008" ["01090"]
      chooseVillain
      choose "no additional upgrade damage" (isLabel $ countLabel "dealDamageToAnEnemy" 3)
      pass
      villainDamageNow `shouldReturn` 3

  it "Living Lie Detector can thwart while in alter-ego form" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      clearHand p
      placeMainThreat 5
      before <- mainSchemeThreatNow
      _ <- paying p "60010" ["01090"]
      chooseMainScheme
      choose "base threat removal" (isLabel $ countLabel "removeThreatFromAScheme" 2)
      pass
      mainSchemeThreatNow `shouldReturn` before - 2

  it "Deposition can decline playing a Sense and still remove two threat" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      clearHand p
      placeMainThreat 5
      before <- mainSchemeThreatNow
      _ <- paying p "60009" ["01090"]
      choose "decline Sense upgrade" (isLabel "$label.done")
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 2
      (length <$> Pack.identitySenseDeck p) `shouldReturn` 5

  it "Foggy Nelson removes two threat while Matt is in alter-ego form" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      clearHand p
      placeMainThreat 5
      before <- mainSchemeThreatNow
      foggy <- paying p "60013" ["01088"]
      choose "Foggy" (cardAbility foggy)
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 2

  it "Daredevil's Billy Club adds one ATK" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- paying p "60017" ["01090"]
      pass
      heroPowers p `shouldReturn` (2,3,2)

  it "returning the Billy Club to hand pays its damage action" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      asHero p
      club <- paying p "60017" ["01090"]
      choose "Billy Club action" (cardAbility club)
      choose "damage" (isLabel "$label.dealDamage")
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 1
      map toCardCode <$> handOf p >>= (`shouldSatisfy` elem "60017")

  it "Sense upgrades leaving play return to the bottom of the Sense deck" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      senses <- Pack.identitySenseDeck p
      let card = testHead senses
      L.runQueueT (L.capture $ Pack.playFromSenseDeck p card True) >>= runNow
      if toCardCode card == "60006" then chooseMainScheme else chooseVillain
      pass
      resolve [RemoveFromPlay $ UpgradeRef $ upgradeOf card]
      returned <- Pack.identitySenseDeck p
      map toCardCode returned `shouldSatisfy` (== toCardCode card) . testHead . reverse
      discardCodes p >>= (`shouldSatisfy` notElem (toCardCode card))

  for_ ["60002","60003","60004","60005","60006"] $ \code ->
    it ("Deposition chooses and plays Sense " <> show code <> " without paying its resource cost") do
      tg <- testGame ["daredevil"]
      inGame tg do
        let p = player1 tg
        clearHand p
        placeMainThreat 4
        senses <- Pack.identitySenseDeck p
        let chosen = testHead $ filter ((== code) . toCardCode) senses
        _ <- paying p "60009" ["01090"]
        choose "chosen Sense" (isTargetChoice chosen)
        unless (code == "60006") chooseVillain
        chooseMainScheme
        pass
        (length <$> upgradesWith code) `shouldReturn` 1
        remaining <- Pack.identitySenseDeck p
        map pcCardId remaining `shouldBe` map pcCardId (filter (/= chosen) senses)

  it "Acute Tactility readies Matt and returns to the bottom before scheme defeat" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      clearHand p
      before <- mainSchemeThreatNow
      when (before < 2) $ placeMainThreat $ 2 - before
      resolve [IdentityMessage p ExhaustedIdentity]
      senses <- Pack.identitySenseDeck p
      let acute = testHead $ filter ((== "60002") . toCardCode) senses
      _ <- paying p "60009" ["01090"]
      choose "Acute Tactility" (isTargetChoice acute)
      chooseMainScheme
      chooseMainScheme
      choose "ready Matt" (cardAbility acute)
      pass
      isExhaustedNow p `shouldReturn` False
      returned <- Pack.identitySenseDeck p
      toCardCode (testHead $ reverse returned) `shouldBe` "60002"

  it "Enhanced Olfaction reduces the next card's cost by two" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      clearHand p
      before <- mainSchemeThreatNow
      when (before < 2) $ placeMainThreat $ 2 - before
      senses <- Pack.identitySenseDeck p
      let scent = testHead $ filter ((== "60003") . toCardCode) senses
      _ <- paying p "60009" ["01090"]
      choose "Enhanced Olfaction" (isTargetChoice scent)
      chooseMainScheme
      chooseMainScheme
      choose "cost reduction" (cardAbility scent)
      pass
      _ <- paying p "60013" []
      pass
      (length <$> supportsWith "60013") `shouldReturn` 1

  it "Radar Sense deals three additional attack damage after attacking its host" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      clearHand p
      senses <- Pack.identitySenseDeck p
      let radar = testHead $ filter ((== "60005") . toCardCode) senses
      L.runQueueT (L.capture $ Pack.playFromSenseDeck p radar True) >>= runNow
      chooseVillain
      pass
      asHero p
      choose "basic attack" (basicAttack p)
      chooseVillain
      choose "Radar Sense" (cardAbility radar)
      pass
      villainDamageNow `shouldReturn` 5

  it "Superior Taste removes two more threat after thwarting its attached scheme" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      clearHand p
      placeMainThreat 6
      senses <- Pack.identitySenseDeck p
      let taste = testHead $ filter ((== "60006") . toCardCode) senses
      L.runQueueT (L.capture $ Pack.playFromSenseDeck p taste True) >>= runNow
      pass
      asHero p
      before <- mainSchemeThreatNow
      choose "basic thwart" (basicThwart p)
      chooseMainScheme
      choose "Superior Taste" (cardAbility taste)
      pass
      mainSchemeThreatNow `shouldReturn` before - 4

  it "Karen Page shuffles a Daredevil card and draws in alter-ego form" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      clearHand p
      discarded <- putInDiscard p "60008"
      karen <- paying p "60014" ["01090"]
      before <- length <$> handOf p
      choose "Karen Page" (cardAbility karen)
      choose "shuffle Cross-Examination" (isTargetChoice discarded)
      pass
      (length <$> handOf p) `shouldReturn` before + 1
      discardCodes p >>= (`shouldSatisfy` notElem "60008")

  it "Sister Maggie increases Matt's recovery by three" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      clearHand p
      _ <- paying p "60016" ["01088"]
      setDamage p 8
      choose "recover" \case UseAbility ab -> abilitySource ab == toSource p && abilityIndex ab == 200; _ -> False
      pass
      damageOf p `shouldReturn` 2

  it "Sister Maggie discards a chosen status after Matt recovers" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      clearHand p
      maggie <- paying p "60016" ["01088"]
      resolve [GiveStatus (IdentityRef p) StunnedStatus]
      setDamage p 8
      choose "recover" \case UseAbility ab -> abilitySource ab == toSource p && abilityIndex ab == 200; _ -> False
      choose "Sister Maggie response" (cardAbility maggie)
      choose "discard stun" (isLabel $ statusLabel StunnedStatus)
      pass
      characterHasCondition StunnedStatus (IdentityCharacter p) `shouldReturn` False
      damageOf p `shouldReturn` 2

  it "The Man Without Fear deals one damage as its cost and readies Daredevil" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      asHero p
      title <- paying p "60018" ["01088"]
      resolve [IdentityMessage p ExhaustedIdentity]
      choose "The Man Without Fear" (cardAbility title)
      whenM (offers $ isTargetChoice p) $ choose "damage Daredevil" (isTargetChoice p)
      choose "ready" (isLabel "$label.ready")
      pass
      damageOf p `shouldReturn` 1
      isExhaustedNow p `shouldReturn` False

  it "aborting a Sense play restores the original ordered deck rather than a hand card" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      asHero p
      senses <- Pack.identitySenseDeck p
      _ <- addMinion p Minions.hydraSoldier
      _ <- putInHand p "01090"
      choose "Superhuman Senses" (isNamedAbility "$label.superhumanSenses")
      unless (toCardCode (testHead senses) == "60006") chooseVillain
      active <- getActiveCost
      for_ active $ \acid -> resolve [AbortPayment acid]
      pass
      restored <- Pack.identitySenseDeck p
      map pcCardId restored `shouldBe` map pcCardId senses
      hand <- handOf p
      map pcCardId hand `shouldSatisfy` notElem (pcCardId $ testHead senses)

  it "Focus the Senses cannot be cleared by Foggy Nelson" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      clearHand p
      focus <- putIntoPlay p "60012"
      foggy <- putIntoPlay p "60013"
      placeMainThreat 3
      choose "Foggy Nelson" (cardAbility foggy)
      let sid = SideSchemeId $ unCardId $ pcCardId focus
      offers (isTargetChoice sid) `shouldReturn` False
      chooseMainScheme
      pass
      sideSchemeThreat sid `shouldReturn` 4

  it "Focus the Senses allows Matt's Deposition to remove threat" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      clearHand p
      focus <- putIntoPlay p "60012"
      _ <- paying p "60009" ["01090"]
      choose "decline Sense" (isLabel "$label.done")
      let sid = SideSchemeId $ unCardId $ pcCardId focus
      choose "Focus the Senses" (isTargetChoice sid)
      pass
      sideSchemeThreat sid `shouldReturn` 2

  it "Raising Hell deals two damage per attached upgrade" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      clearHand p
      senses <- Pack.identitySenseDeck p
      for_ ["60002","60005"] $ \code -> do
        let sense = testHead $ filter ((== code) . toCardCode) senses
        L.runQueueT (L.capture $ Pack.playFromSenseDeck p sense True) >>= runNow
        chooseVillain
        pass
      asHero p
      _ <- paying p "60011" ["01088"]
      pass
      villainDamageNow `shouldReturn` 4

  it "Raising Hell deals three damage per attached upgrade while Aerial" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      clearHand p
      senses <- Pack.identitySenseDeck p
      for_ ["60002","60005"] $ \code -> do
        let sense = testHead $ filter ((== code) . toCardCode) senses
        L.runQueueT (L.capture $ Pack.playFromSenseDeck p sense True) >>= runNow
        chooseVillain
        pass
      asHero p
      club <- paying p "60017" ["01090"]
      choose "Billy Club action" (cardAbility club)
      choose "gain Aerial" (isLabel "$label.gainAerial")
      _ <- paying p "60011" ["01088"]
      pass
      villainDamageNow `shouldReturn` 6

  it "Cross-Examination can select one bonus damage per attached upgrade" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      clearHand p
      senses <- Pack.identitySenseDeck p
      let sense = testHead $ filter ((== "60005") . toCardCode) senses
      L.runQueueT (L.capture $ Pack.playFromSenseDeck p sense True) >>= runNow
      chooseVillain
      pass
      asHero p
      _ <- paying p "60008" ["01090"]
      chooseVillain
      choose "one additional damage" (isLabel $ countLabel "dealDamageToAnEnemy" 4)
      pass
      villainDamageNow `shouldReturn` 4

  it "Raising Hell adds Aggressive Energy once per enemy rather than once per upgrade" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      clearHand p
      senses <- Pack.identitySenseDeck p
      for_ ["60002","60005"] $ \code -> do
        let sense = testHead $ filter ((== code) . toCardCode) senses
        L.runQueueT (L.capture $ Pack.playFromSenseDeck p sense True) >>= runNow
        chooseVillain
        pass
      asHero p
      _ <- paying p "60011" ["32047","01071"]
      choose "Aggressive Energy" (isLabel "$label.aggressiveEnergy")
      pass
      villainDamageNow `shouldReturn` 5

  it "Elektra redirects consequential damage to Daredevil in hero form" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      asHero p
      elektra <- putIntoPlay p "60007"
      attrs <- allyIn "60007"
      choose "Elektra attacks" (isAllyAttack attrs)
      chooseVillain
      choose "Elektra forced interrupt" (cardAbility elektra)
      pass
      damageOf p `shouldReturn` 1
      (allyDamage <$> allyIn "60007") `shouldReturn` 0

  it "Raising Hell cannot be played when no enemy has an attached upgrade" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      asHero p
      event <- putInHand p "60011"
      _ <- putInHand p "01088"
      offers (isPlay event) `shouldReturn` False

  it "a stunned Daredevil can initiate Raising Hell to discard his stun status" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      asHero p
      resolve [GiveStatus (IdentityRef p) StunnedStatus]
      _ <- paying p "60011" ["01088"]
      pass
      characterHasCondition StunnedStatus (IdentityCharacter p) `shouldReturn` False
      villainDamageNow `shouldReturn` 0
