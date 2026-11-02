module Marvel.Aspect.FearNoEvilPlayerSpec (spec) where

import Marvel.CampaignPlayerTest
import Marvel.Minion.Cards qualified as Minions
import Marvel.Status
import TestImport

spec :: Spec
spec = describe "Fear No Evil player cards" do
  it "Stick exhausts to increase a Martial Artist's basic attack for that attack" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      asHero p
      stick <- putIntoPlay p "60029"
      choose "basic attack" (basicAttack p)
      choose "Stick's bonus" \choice -> cardAbility stick choice && case choice of
        UseAbility ab -> abilityIndex ab == 1
        _ -> False
      offers (isAbilityOfCard stick) `shouldReturn` False
      pass
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 3
      heroPowers p `shouldReturn` (1,2,2)

  it "Stick cannot also boost the same basic attack after its readying option" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      asHero p
      stick <- putIntoPlay p "60029"
      resolve [SupportMessage (supportIdOf stick) ExhaustedSupport]
      choose "basic attack" (basicAttack p)
      choose "Stick's readying option" \choice -> isAbilityOfCard stick choice && case choice of
        UseAbility ab -> abilityIndex ab == 2
        _ -> False
      offers (isAbilityOfCard stick) `shouldReturn` False
      pass
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 1
      sticks <- supportsWith "60029"
      map supportExhausted sticks `shouldBe` [False]
      heroPowers p `shouldReturn` (1,2,2)

  it "Daredevil does not discount an event after defending because allies have no basic DEF power" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      daredevil <- putIntoPlay p "60039"
      mid <- addMinion p Minions.hydraSoldier
      runNow [MinionMessage mid $ MinionAttacks p]
      choose "Daredevil defends" \case AllyDefend aid _ -> aid == allyIdOf daredevil; _ -> False
      offers (cardAbility daredevil) `shouldReturn` False
      pass
      (allyDamage <$> allyIn "60039") `shouldReturn` 2
      event <- putInHand p "01087"
      _ <- putInHand p "01071"
      offers (isPlay event) `shouldReturn` False

  it "the Daredevil Team-Up cards require their other named character in play" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      asHero p
      dance <- putInHand p "60031"
      seeHear <- putInHand p "60055"
      _ <- putInHand p "01088"
      offers (isPlay dance) `shouldReturn` False
      offers (isPlay seeHear) `shouldReturn` False

  it "Move in Shadow responds to its own play with one threat removal" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      asHero p
      placeMainThreat 5
      before <- mainSchemeThreatNow
      shadow <- paying p "60027" ["01088"]
      choose "Move in Shadow response" (cardAbility shadow)
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 1

  it "Dance with the Devil discards itself to attack its attached enemy" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "60007"
      dance <- putInHand p "60031"
      playWith dance []
      chooseVillain
      pass
      choose "Dance with the Devil" (cardAbility dance)
      pass
      villainDamageNow `shouldReturn` 3
      discardCodes p >>= (`shouldSatisfy` elem "60031")

  it "See No Evil, Hear No Evil may choose its attack option twice" do
    tg <- testGame ["daredevil","echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      resolve [IdentityMessage (player2 tg) ChooseHeroForm]
      pass
      _ <- paying p "60055" ["01088"]
      choose "first attack" (isLabel "$label.dealDamage")
      chooseVillain
      choose "second attack" (isLabel "$label.dealDamage")
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 6

  it "Blindspot confuses an upgraded enemy after thwarting" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      asHero p
      placeMainThreat 5
      mid <- addMinion p Minions.hydraBomber
      trouble <- putInHand p "60026"
      payment <- putInHand p "01071"
      playOn trouble mid [payment]
      pass
      card <- putIntoPlay p "60019"
      blindspot <- allyIn "60019"
      choose "Blindspot thwarts" (isAllyThwart blindspot)
      chooseMainScheme
      choose "Blindspot response" (cardAbility card)
      choose "confuse the upgraded minion" (isTargetChoice mid)
      pass
      characterHasCondition ConfusedStatus (MinionCharacter mid) `shouldReturn` True

  it "Ghost Rider spends one energy to confuse a minion before attacking it" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      asHero p
      mid <- addMinion p Minions.armoredGuard
      card <- putIntoPlay p "60022"
      ghost <- allyIn "60022"
      energy <- putInHand p "01088"
      choose "Ghost Rider attacks" (isAllyAttack ghost)
      choose "attack the minion" (isTargetChoice mid)
      choose "Ghost Rider interrupt" (cardAbility card)
      payWith [energy]
      pass
      characterHasCondition ConfusedStatus (MinionCharacter mid) `shouldReturn` True

  it "American Sign Language exhausts to generate a wild resource for an event" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      language <- putIntoPlay p "60043"
      haymaker <- putInHand p "01087"
      mental <- putInHand p "01071"
      choose "Haymaker" (isPlay haymaker)
      choose "American Sign Language resource" (cardAbility language)
      payWith [mental]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 3
      languages <- upgradesWith "60043"
      map upgradeExhausted languages `shouldBe` [True]

  it "Nelson and Murdock confuses an enemy after Foggy defeats a side scheme" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      clearHand p
      office <- putIntoPlay p "60015"
      foggy <- putIntoPlay p "60013"
      scheme <- putIntoPlay p "60024"
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      resolve [SideSchemeMessage sid $ SideSchemeThreatRemoved (toSource GameRef) 2]
      choose "Foggy removes the remaining threat" (cardAbility foggy)
      choose "defeat De-escalation" (isTargetChoice sid)
      choose "Nelson and Murdock response" (cardAbility office)
      chooseVillain
      pass
      villain <- villainIdNow
      characterHasCondition ConfusedStatus (VillainCharacter villain) `shouldReturn` True

  it "Army of One readies an exhausted hero" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      resolve [IdentityMessage p ExhaustedIdentity]
      _ <- paying p "60048" ["01090"]
      pass
      isExhaustedNow p `shouldReturn` False

  it "Army of One costs one additional resource for each controlled ally" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "60039"
      resolve [IdentityMessage p ExhaustedIdentity]
      event <- putInHand p "60048"
      _ <- putInHand p "01071"
      offers (isPlay event) `shouldReturn` False

  it "The Best Offense replaces ATK and THW with modified DEF" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- paying p "60052" ["01090"]
      pass
      heroPowers p `shouldReturn` (3,3,3)

  it "Ronin grants DEF while no allies are controlled" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- paying p "60053" ["01090"]
      pass
      heroPowers p `shouldReturn` (2,2,3)
      _ <- putIntoPlay p "60039"
      heroPowers p `shouldReturn` (2,2,2)

  it "Know Your Enemy removes one threat twice after the Martial Artist discount" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      asHero p
      placeMainThreat 5
      before <- mainSchemeThreatNow
      _ <- paying p "60023" []
      chooseMainScheme
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 2

  it "Legal Trouble reduces its attached minion's SCH by two" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      clearHand p
      mid <- addMinion p Minions.hydraBomber
      trouble <- putInHand p "60026"
      playOn trouble mid []
      pass
      attached <- upgradesWith "60026"
      map upgradeAttachedEnemy attached `shouldBe` [Just $ EnemyMinionId mid]

  it "In Harm's Way uses modified DEF for both damage and threat removal" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      placeMainThreat 6
      before <- mainSchemeThreatNow
      _ <- paying p "60050" ["01088","01090"]
      chooseVillain
      chooseMainScheme
      pass
      villainDamageNow `shouldReturn` 2
      mainSchemeThreatNow `shouldReturn` before - 2

  it "Cloak contributes an acceleration icon until Dagger is in play" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      clearHand p
      before <- getAccelerationCount
      _ <- putIntoPlay p "60020"
      getAccelerationCount `shouldReturn` before + 1
      _ <- putIntoPlay p "60021"
      getAccelerationCount `shouldReturn` before

  it "Dagger takes two consequential damage without Cloak" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "60021"
      dagger <- allyIn "60021"
      choose "Dagger attacks" (isAllyAttack dagger)
      chooseVillain
      pass
      (allyDamage <$> allyIn "60021") `shouldReturn` 2

  it "Cloak reduces Dagger's consequential damage by one" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "60020"
      _ <- putIntoPlay p "60021"
      dagger <- allyIn "60021"
      choose "Dagger attacks" (isAllyAttack dagger)
      chooseVillain
      pass
      (allyDamage <$> allyIn "60021") `shouldReturn` 1

  it "Cloak finds Dagger and puts her into play rather than playing her" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      clearHand p
      cloak <- putIntoPlay p "60020"
      _ <- putInDiscard p "60021"
      energy <- putInHand p "01088"
      choose "Cloak action" (cardAbility cloak)
      payWith [energy]
      pass
      (length <$> alliesWith "60021") `shouldReturn` 1

  it "Stealth Training stuns after an exactly defeated side scheme" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      training <- putIntoPlay p "60028"
      scheme <- putIntoPlay p "60024"
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      resolve [SideSchemeMessage sid $ SideSchemeThreatRemoved GameRef 2]
      _ <- paying p "60023" []
      choose "first thwart" (isTargetChoice sid)
      choose "second thwart" (isTargetChoice sid)
      choose "Stealth Training" (cardAbility training)
      chooseVillain
      pass
      vid <- villainIdNow
      characterHasCondition StunnedStatus (VillainCharacter vid) `shouldReturn` True

  it "Contingency Planning tucks a legal upgrade even before its host enters play" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      clearHand p
      plan <- putIntoPlay p "60030"
      trouble <- putInHand p "60026"
      choose "Contingency Planning" (cardAbility plan)
      choose "tuck Legal Trouble" (isTargetChoice trouble)
      pass
      held <- upgradesWith "60030"
      map (map toCardCode . upgradeHeldCards) held `shouldBe` [["60026"]]
      map upgradeHeldCardsFacedown held `shouldBe` [False]
      offers (cardAbility plan) `shouldReturn` False
      map toCardCode <$> handOf p >>= (`shouldSatisfy` notElem "60026")
      _ <- addMinion p Minions.hydraBomber
      offers (isPlay trouble) `shouldReturn` True

  it "Contingency Planning cannot tuck an upgrade that attaches only to a hero" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      clearHand p
      plan <- putIntoPlay p "60030"
      _ <- putInHand p "01081"
      offers (cardAbility plan) `shouldReturn` False

  it "The Best Offense ignores ATK bonuses from Savage Strike" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "60052"
      strike <- putInHand p "56017"
      payments <- traverse (putInHand p) ["01088","01090"]
      choose "basic attack" (basicAttack p)
      playWith strike payments
      pass
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 3

  it "Stand Alone readies the hero when attacked without any controlled allies" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      alone <- putIntoPlay p "60054"
      resolve [IdentityMessage p ExhaustedIdentity]
      vid <- villainIdNow
      start [VillainMessage vid $ VillainAttacksWithoutBoost p]
      choose "Stand Alone" (cardAbility alone)
      pass
      isExhaustedNow p `shouldReturn` False
      damageOf p `shouldReturn` 2

  it "Stand Alone cannot trigger while an ally is controlled" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      alone <- putIntoPlay p "60054"
      _ <- putIntoPlay p "60039"
      resolve [IdentityMessage p ExhaustedIdentity]
      vid <- villainIdNow
      start [VillainMessage vid $ VillainAttacksWithoutBoost p]
      offers (cardAbility alone) `shouldReturn` False

  it "Get Their Attention removes three threat as a defense without basic DEF" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      placeMainThreat 5
      before <- mainSchemeThreatNow
      event <- putInHand p "60049"
      payment <- putInHand p "01088"
      vid <- villainIdNow
      start [VillainMessage vid $ VillainAttacksWithoutBoost p]
      playWith event [payment]
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 3
      damageOf p `shouldReturn` 2

  it "De-escalation removes a physical acceleration token and enters victory" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      scheme <- putIntoPlay p "60024"
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      mid <- mainSchemeIdNow
      resolve [PlaceAccelerationToken mid]
      before <- getAccelerationTokenCount
      choose "first basic thwart" (basicThwart p)
      choose "De-escalation" (isTargetChoice sid)
      pass
      _ <- paying p "60048" ["01071"]
      pass
      choose "second basic thwart" (basicThwart p)
      choose "De-escalation" (isTargetChoice sid)
      pass
      choose "remove the main scheme's acceleration token" (isTargetChoice mid)
      pass
      getAccelerationTokenCount `shouldReturn` before - 1
      map toCardCode <$> getPlayerVictoryDisplay >>= (`shouldSatisfy` elem "60024")

  it "Cloak can find Dagger in another player's hand while preserving her owner" do
    tg <- testGame ["daredevil","spider-man"]
    inGame tg do
      let p = player1 tg
          other = player2 tg
      clearHand p
      cloak <- putIntoPlay p "60020"
      dagger <- putInHand other "60021"
      energy <- putInHand p "01088"
      choose "Cloak" (cardAbility cloak)
      payWith [energy]
      choose "the other player's Dagger" (isTargetChoice dagger)
      pass
      attrs <- allyIn "60021"
      allyController attrs `shouldBe` p
      allyOwner attrs `shouldBe` other
      map pcCardId <$> handOf other >>= (`shouldSatisfy` notElem (pcCardId dagger))

  it "Cloak cannot make an already in-play Dagger reenter play" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      clearHand p
      cloak <- putIntoPlay p "60020"
      dagger <- putIntoPlay p "60021"
      resolve [AllyMessage (allyIdOf dagger) $ AllyDamaged GameRef $ toDamage 1 FromAbility]
      _ <- putInHand p "01088"
      offers (cardAbility cloak) `shouldReturn` False
      (allyDamage <$> allyIn "60021") `shouldReturn` 1

  it "Cloak's action is unavailable when no Dagger is in any eligible game zone" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      clearHand p
      cloak <- putIntoPlay p "60020"
      _ <- putInHand p "01088"
      offers (cardAbility cloak) `shouldReturn` False

  it "Cloak takes no consequential damage after his basic powers" do
    tg <- testGame ["daredevil"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "60020"
      cloak <- allyIn "60020"
      choose "Cloak attacks" (isAllyAttack cloak)
      chooseVillain
      pass
      (allyDamage <$> allyIn "60020") `shouldReturn` 0
