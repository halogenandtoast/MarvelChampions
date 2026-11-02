module Marvel.Hero.TigraSpec (spec) where

import Marvel.CampaignPlayerTest
import Marvel.Minion.Cards qualified as Minions
import Marvel.Message.Lifted qualified as L
import Marvel.Status
import TestImport

spec :: Spec
spec = describe "Tigra" do
  it "starts as Greer Nelson with six cards and ten hit points" do
    tg <- testGame ["tigra"]
    inGame tg do
      handSizeOf (player1 tg) `shouldReturn` 6
      hitPointsOf (player1 tg) `shouldReturn` 10

  it "has the printed hero basic powers" do
    tg <- testGame ["tigra"]
    inGame tg do
      asHero (player1 tg)
      heroPowers (player1 tg) `shouldReturn` (1,2,3)

  it "Greer can find Hunted once per round" do
    tg <- testGame ["tigra"]
    inGame tg do
      let p = player1 tg
      clearHand p
      hunted <- putInDiscard p "56007"
      choose "Undercover Work" (isNamedAbility "$label.undercoverWork")
      choose "Hunted" (isTargetChoice hunted)
      pass
      (map toCardCode <$> handOf p) >>= (`shouldSatisfy` elem "56007")
      offers (isNamedAbility "$label.undercoverWork") `shouldReturn` False

  it "Tooth and Claw makes two separately chosen attacks" do
    tg <- testGame ["tigra"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- paying p "56008" ["01088","01089"]
      chooseVillain
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 8

  it "Feline Senses removes three threat" do
    tg <- testGame ["tigra"]
    inGame tg do
      let p = player1 tg
      asHero p
      placeMainThreat 5
      before <- mainSchemeThreatNow
      _ <- paying p "56009" ["01088"]
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 3

  it "Precinct HQ scales its alter-ego threat removal with engaged minions" do
    tg <- testGame ["tigra"]
    inGame tg do
      let p = player1 tg
      clearHand p
      _ <- addMinion p Minions.hydraBomber
      placeMainThreat 5
      before <- mainSchemeThreatNow
      hq <- paying p "56003" ["01088"]
      choose "Precinct HQ" (cardAbility hq)
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 2

  it "Moon Knight puts a searched minion into play stunned and confused" do
    tg <- testGame ["tigra"]
    inGame tg do
      let p = player1 tg
      asHero p
      (deck,_) <- setEncounterDeck [Minions.hydraBomber, Minions.armoredGuard] []
      moon <- paying p "56002" ["01071"]
      choose "Moon Knight" (cardAbility moon)
      choose "searched minion" (isTargetChoice $ testHead deck)
      pass
      let mid = MinionId $ unCardId $ ecCardId $ testHead deck
      characterHasCondition StunnedStatus (MinionCharacter mid) `shouldReturn` True
      characterHasCondition ConfusedStatus (MinionCharacter mid) `shouldReturn` True

  it "Sharp Claws adds one ATK and piercing for the selected attack" do
    tg <- testGame ["tigra"]
    inGame tg do
      let p = player1 tg
      asHero p
      claws <- paying p "56005" ["01090"]
      vid <- villainIdNow
      resolve [GiveStatus (VillainRef vid) ToughStatus]
      choose "basic attack" (basicAttack p)
      choose "Sharp Claws" (cardAbility claws)
      choose "piercing" (isLabel "$label.piercing")
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 3
      heroPowers p `shouldReturn` (1,2,3)

  it "Cat-Like Reflexes prevents three damage and may confuse an engaged minion" do
    tg <- testGame ["tigra"]
    inGame tg do
      let p = player1 tg
      asHero p
      reflexes <- paying p "56006" ["01090"]
      mid <- addMinion p Minions.hydraBomber
      setDamage p 0
      L.runQueueT (L.capture $ L.damageIdentity p GameRef $ toDamage 5 FromAbility) >>= runNow
      choose "Cat-Like Reflexes" (cardAbility reflexes)
      choose "confuse minion" (isTargetChoice mid)
      pass
      damageOf p `shouldReturn` 2
      characterHasCondition ConfusedStatus (MinionCharacter mid) `shouldReturn` True
      discardCodes p >>= (`shouldSatisfy` elem "56006")

  it "Hunted attaches to the minion found as its additional cost and stuns it" do
    tg <- testGame ["tigra"]
    inGame tg do
      let p = player1 tg
      asHero p
      (deck,_) <- setEncounterDeck [Minions.hydraBomber, Minions.armoredGuard] []
      hunted <- putInHand p "56007"
      playWith hunted []
      choose "find the minion" (isTargetChoice $ testHead deck)
      whenM (offers isFinishPayment) $ choose "finish payment" isFinishPayment
      pass
      let mid = MinionId $ unCardId $ ecCardId $ testHead deck
      attached <- upgradesWith "56007"
      map upgradeAttachedEnemy attached `shouldBe` [Just $ EnemyMinionId mid]
      characterHasCondition StunnedStatus (MinionCharacter mid) `shouldReturn` True

  it "Feline Senses can stun a minion after removing a scheme's last threat" do
    tg <- testGame ["tigra"]
    inGame tg do
      let p = player1 tg
      asHero p
      mid <- addMinion p Minions.hydraBomber
      before <- mainSchemeThreatNow
      when (before < 3) $ placeMainThreat $ 3 - before
      _ <- paying p "56009" ["01088"]
      chooseMainScheme
      choose "stun the minion" (isTargetChoice mid)
      pass
      characterHasCondition StunnedStatus (MinionCharacter mid) `shouldReturn` True

  it "Cat's Head Amulet produces one physical resource per engaged minion" do
    tg <- testGame ["tigra"]
    inGame tg do
      let p = player1 tg
      asHero p
      amulet <- putIntoPlay p "56004"
      _ <- addMinion p Minions.hydraBomber
      event <- putInHand p "01087"
      payment <- putInHand p "01071"
      playWith event []
      choose "Cat's Head Amulet" (cardAbility amulet)
      payWith [payment]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 3
      upgradeIsExhausted (upgradeOf amulet) `shouldReturn` True

  it "Feline Senses does not offer another identity's protected Focus the Senses" do
    tg <- testGame ["tigra","daredevil"]
    inGame tg do
      let p = player1 tg
      asHero p
      focus <- putIntoPlay (player2 tg) "60012"
      let sid = SideSchemeId $ unCardId $ pcCardId focus
      mid <- addMinion p Minions.hydraBomber
      placeMainThreat 5
      _ <- paying p "56009" ["01088"]
      offers (isTargetChoice sid) `shouldReturn` False
      chooseMainScheme
      pass
      characterHasCondition StunnedStatus (MinionCharacter mid) `shouldReturn` False
      sideSchemeThreat sid `shouldReturn` 4

  it "Feline Senses does not stun when a replacement reduces the actual removal" do
    tg <- testGame ["tigra"]
    inGame tg do
      let p = player1 tg
      asHero p
      mid <- addMinion p Minions.hydraBomber
      _ <- putIntoPlay p "60024"
      main <- mainSchemeIdNow
      overwatch <- putInHand p "30019"
      playOn overwatch main []
      before <- mainSchemeThreatNow
      when (before < 3) $ placeMainThreat $ 3 - before
      _ <- paying p "56009" ["01088"]
      chooseMainScheme
      offers (cardAbility overwatch) `shouldReturn` True
      reducePendingThwart (MainSchemeRef main) 2
      pass
      mainSchemeThreatNow `shouldReturn` 2
      offers (isTargetChoice mid) `shouldReturn` False
      characterHasCondition StunnedStatus (MinionCharacter mid) `shouldReturn` False
