module Marvel.Aspect.RoguePackSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromJust)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Stats (unAtk, unThw)
import Marvel.Window qualified as W
import TestImport

cardAbility :: PlayerCard -> Natural -> Choice -> Bool
cardAbility card n choice = isAbilityOfCard card choice && case choice of
  UseAbility ab -> abilityIndex ab == n
  _ -> False

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> clearHand p

villainAttacksWithBoost :: IdentityId -> CardDef -> GameAppT ()
villainAttacksWithBoost p boost = do
  _ <- stackEncounterDeck [boost]
  vid <- villainIdNow
  runNow [VillainMessage vid (VillainAttacks p)]

minionStunnedNow :: MinionId -> GameAppT Bool
minionStunnedNow mid = getsGame $ maybe False (minionStunned . toAttrs) . HashMap.lookup mid . gameMinions

usesNow :: PlayerCard -> GameAppT Natural
usesNow card = getsGame $ maybe 0 (upgradeUses . toAttrs) . HashMap.lookup (upgradeIdOf card) . gameUpgrades

spec :: Spec
spec = describe "Rogue pack" do
  describe "Iceman" do
    it "enters with three freeze counters and may stun a minion entering for any player" do
      tg <- testGame ["rogue", "spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        ice <- putIntoPlay p "38010"
        getCountersOn (allyIdOf ice) `shouldReturn` 3
        enemy <- minionOf <$> revealNow (player2 tg) Minions.hydraMercenary
        choose "spend a freeze counter" (cardAbility ice 1)
        pass
        minionStunnedNow enemy `shouldReturn` True
        getCountersOn (allyIdOf ice) `shouldReturn` 2

    it "may decline the response and cannot use it without freeze counters" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        asHero p
        ice <- putIntoPlay p "38010"
        enemy <- minionOf <$> revealNow p Minions.hydraMercenary
        pass
        minionStunnedNow enemy `shouldReturn` False
        getCountersOn (allyIdOf ice) `shouldReturn` 3
        resolve [RemoveCounters (AllyRef $ allyIdOf ice) 3]
        _ <- revealNow p Minions.hydraSoldier
        offers (cardAbility ice 1) `shouldReturn` False

  describe "Armor" do
    it "requires X-Men and enters with tough and her printed powers" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        clearHand p
        armor <- putInHand p "38012"
        energy <- putInHand p "01088"
        offers (isPlay armor) `shouldReturn` False
        changeForm p
        playWith armor [energy]
        a <- allyIn "38012"
        allyTough a `shouldBe` True
        unThw (allyThwart a) `shouldBe` 1
        unAtk (allyAttack a) `shouldBe` 1

    it "cannot be played by an identity without X-Men" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        armor <- putInHand p "38012"
        _ <- putInHand p "01088"
        offers (isPlay armor) `shouldReturn` False

  describe "Karma" do
    it "takes a non-Elite minion after being played from hand and restores it when Karma leaves" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        asHero p
        mid <- addMinion p Minions.hydraMercenary
        elite <- addMinion p Minions.sandman
        card <- putInHand p "38011"
        energy <- putInHand p "01088"
        genius <- putInHand p "01089"
        playWith card [energy, genius]
        choose "Karma's played response" (cardAbility card 1)
        offers (isTargetChoice elite) `shouldReturn` False
        choose "control the mercenary" (isTargetChoice mid)
        getsGame (HashMap.member mid . gameMinions) `shouldReturn` False
        let aid = AllyId $ unMinionId mid
        host <- getsGame $ toAttrs . fromJust . HashMap.lookup aid . gameAllies
        allyAttackConsequentialDamage host `shouldBe` 2
        allyThwartConsequentialDamage host `shouldBe` 2
        resolve [ReturnToHand $ AllyRef $ allyIdOf card]
        getsGame (HashMap.member mid . gameMinions) `shouldReturn` True
        getsGame (HashMap.member aid . gameAllies) `shouldReturn` False

    it "does not respond when put directly into play or when no non-Elite minion exists" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- addMinion p Minions.hydraMercenary
        card <- putIntoPlay p "38011"
        offers (cardAbility card 1) `shouldReturn` False
      other <- testGame ["rogue"]
      inGame other do
        let q = player1 other
        asHero q
        _ <- addMinion q Minions.sandman
        card <- putInHand q "38011"
        first <- putInHand q "01088"
        second <- putInHand q "01089"
        playWith card [first, second]
        offers (cardAbility card 1) `shouldReturn` False

    it "preserves damage and statuses and pays two consequential damage after attacking" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        asHero p
        mid <- addMinion p Minions.hydraSoldier
        resolve [MinionMessage mid $ MinionDamaged (IdentityRef p) (toDamage 1 FromAbility)]
        card <- putInHand p "38011"
        first <- putInHand p "01088"
        second <- putInHand p "01089"
        playWith card [first, second]
        choose "Karma" (cardAbility card 1)
        choose "control soldier" (isTargetChoice mid)
        let aid = AllyId $ unMinionId mid
        host <- getsGame $ toAttrs . fromJust . HashMap.lookup aid . gameAllies
        allyDamage host `shouldBe` 1
        choose "controlled minion attacks" (isAllyAttack host)
        chooseVillain
        hostAfter <- getsGame $ toAttrs . fromJust . HashMap.lookup aid . gameAllies
        allyDamage hostAfter `shouldBe` 3

  describe "Judoka Skill" do
    it "reduces only the defended attack and discards after its third judo counter" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        asHero p
        skill <- putIntoPlay p "38014"
        usesNow skill `shouldReturn` 3
        resolve [RemoveCounters (UpgradeRef $ upgradeIdOf skill) 2]
        villainAttacksWithBoost p Minions.vulture
        choose "defend" isDefend
        choose "spend final judo counter" (cardAbility skill 1)
        pass
        damageOf p `shouldReturn` 0
        discardOf p >>= (`shouldSatisfy` elem skill)
        resolve [IdentityMessage p ReadiedIdentity]
        villainAttacksWithBoost p Minions.vulture
        choose "defend the next attack" isDefend
        pass
        damageOf p `shouldReturn` 2

    it "does not trigger without defending and allows only one copy" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        asHero p
        skill <- putIntoPlay p "38014"
        duplicate <- putInHand p "38014"
        _ <- putInHand p "01088"
        offers (isPlay duplicate) `shouldReturn` False
        mid <- addMinion p Minions.hydraMercenary
        runNow [MinionMessage mid $ MinionAttacks p]
        choose "take attack" isNoDefender
        offers (cardAbility skill 1) `shouldReturn` False
        usesNow skill `shouldReturn` 3

  describe "Not Today!" do
    it "adds two DEF for that attack and removes two threat when no attack damage is taken" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 4
        before <- mainSchemeThreatNow
        event <- putInHand p "38016"
        resource <- putInHand p "01090"
        villainAttacksWithBoost p Minions.vulture
        choose "defend" isDefend
        playWith event [resource]
        chooseMainScheme
        damageOf p `shouldReturn` 0
        mainSchemeThreatNow `shouldReturn` (before - 2)

    it "does not remove threat when attack damage remains" do
      tg <- testGame ["quicksilver"]
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 4
        before <- mainSchemeThreatNow
        event <- putInHand p "38016"
        resource <- putInHand p "01090"
        villainAttacksWithBoost p Minions.vulture
        choose "defend" isDefend
        playWith event [resource]
        pass
        damageOf p `shouldReturn` 1
        mainSchemeThreatNow `shouldReturn` before

    it "removes threat when tough prevents the remaining damage" do
      tg <- testGame ["quicksilver"]
      let p = player1 tg
      inGame tg do
        asHero p
        resolve [IdentityMessage p IdentityBecomeTough]
        placeMainThreat 4
        before <- mainSchemeThreatNow
        event <- putInHand p "38016"
        resource <- putInHand p "01090"
        villainAttacksWithBoost p Minions.vulture
        choose "defend" isDefend
        playWith event [resource]
        chooseMainScheme
        damageOf p `shouldReturn` 0
        mainSchemeThreatNow `shouldReturn` (before - 2)

  describe "Moira MacTaggert" do
    it "requires Mutant to play and draws after that alter-ego changes to hero" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        clearHand p
        moira <- putInHand p "38018"
        energy <- putInHand p "01088"
        playWith moira [energy]
        top <- testHead <$> stackDeck p ["38005"]
        changeForm p
        choose "Moira sees the Mutant change" (cardAbility moira 1)
        handOf p `shouldReturn` [top]
        supportsWith "38018" >>= (`shouldSatisfy` all supportExhausted)

    it "does not respond to a non-Mutant form change or permit play without Mutant" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putInHand p "38018"
        _ <- putInHand p "01088"
        offers (isPlay card) `shouldReturn` False
        moira <- putIntoPlay p "38018"
        changeForm p
        offers (cardAbility moira 1) `shouldReturn` False

    it "draws for another player's Mutant and exhausts its controller's support" do
      tg <- testGame ["rogue", "gambit"]
      let p = player1 tg
          q = player2 tg
      inGame tg do
        clearHand p
        clearHand q
        moira <- putIntoPlay p "38018"
        top <- testHead <$> stackDeck q ["37006"]
        runNow [IdentityMessage q $ ChangedToForm A]
        choose "Moira sees Gambit's change" (cardAbility moira 1)
        handOf q `shouldReturn` [top]
        length <$> handOf p `shouldReturn` 0

  describe "X-Gene" do
    it "generates a wild for identity-specific events and exhausts" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        clearHand p
        gene <- putInHand p "38019"
        resource <- putInHand p "01090"
        playWith gene [resource]
        changeForm p
        attack <- putInHand p "38006"
        energy <- putInHand p "01088"
        choose "play Southern Cross" (isPlay attack)
        choose "X-Gene generates wild" (cardAbility gene 1)
        payWith [energy]
        chooseVillain
        villainDamageNow `shouldReturn` 6
        upgradesWith "38019" >>= (`shouldSatisfy` all upgradeExhausted)

    it "cannot pay for aspect events, basic cards, or a second X-Gene" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        clearHand p
        gene <- putIntoPlay p "38019"
        duplicate <- putInHand p "38019"
        _ <- putInHand p "01088"
        offers (isPlay duplicate) `shouldReturn` False
        basic <- putInHand p "01084"
        _ <- putInHand p "01089"
        choose "play basic support" (isPlay basic)
        offers (cardAbility gene 1) `shouldReturn` False

  describe "Med Lab" do
    it "captures an ally defeated by consequential damage and later plays it exhausted with normal payment" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        asHero p
        lab <- putIntoPlay p "38028"
        ice <- putIntoPlay p "38010"
        resolve [AllyMessage (allyIdOf ice) $ AllyDamaged (IdentityRef p) (toDamage 2 FromAbility)]
        a <- allyIn "38010"
        choose "Iceman attacks" (isAllyAttack a)
        chooseVillain
        choose "Med Lab captures him" (cardAbility lab 1)
        alliesWith "38010" `shouldReturn` []
        discardOf p >>= (`shouldSatisfy` notElem ice)
        resolve [SupportMessage (supportIdOf lab) ReadiedSupport, IdentityMessage p $ ChangedToForm B]
        energy <- putInHand p "01088"
        strength <- putInHand p "01090"
        choose "Med Lab's paid replay" (cardAbility lab 2)
        choose "play held Iceman" (isTargetChoice ice)
        payWith [energy, strength]
        revived <- allyIn "38010"
        allyExhausted revived `shouldBe` True
        allyDamage revived `shouldBe` 0
        getCountersOn (allyIdOf ice) `shouldReturn` 3
        supportsWith "38028" >>= (`shouldSatisfy` all (null . supportHeldCards))

    it "does not capture allies defeated by other damage and cannot replay without resources" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        asHero p
        lab <- putIntoPlay p "38028"
        ice <- putIntoPlay p "38010"
        runNow [AllyMessage (allyIdOf ice) $ AllyDamaged (IdentityRef p) (toDamage 3 FromAbility)]
        offers (cardAbility lab 1) `shouldReturn` False
        pass
        supportsWith "38028" >>= (`shouldSatisfy` all (null . supportHeldCards))
        resolve [IdentityMessage p $ ChangedToForm B]
        offers (cardAbility lab 2) `shouldReturn` False

    it "holds only one ally, preserves its owner, and requires affordable normal costs" do
      tg <- testGame ["rogue", "spider-man"]
      let p = player1 tg
          q = player2 tg
      inGame tg do
        asHero p
        lab <- putIntoPlay p "38028"
        ice <- putIntoPlay q "38010"
        resolve [AllyMessage (allyIdOf ice) $ AllyDamaged (IdentityRef p) (toDamage 2 FromAbility)]
        a <- allyIn "38010"
        runNow [AllyMessage (allyId a) AllyAttacked]
        chooseVillain
        choose "capture another player's ally" (cardAbility lab 1)
        resolve [SupportMessage (supportIdOf lab) ReadiedSupport]
        another <- putIntoPlay p "01067"
        resolve [AllyMessage (allyIdOf another) $ AllyDamaged (IdentityRef p) (toDamage 1 FromAbility)]
        b <- allyIn "01067"
        choose "another ally attacks" (isAllyAttack b)
        chooseVillain
        offers (cardAbility lab 1) `shouldReturn` False
        pass
        resolve [IdentityMessage p $ ChangedToForm B]
        clearHand p
        offers (cardAbility lab 2) `shouldReturn` False
        first <- putInHand p "01088"
        second <- putInHand p "01090"
        choose "play captured ally" (cardAbility lab 2)
        choose "Iceman" (isTargetChoice ice)
        payWith [first, second]
        revived <- allyIn "38010"
        allyOwner revived `shouldBe` q
        allyController revived `shouldBe` p
        allyExhausted revived `shouldBe` True

  describe "Rogue pack restrictions" do
    it "X-Gene cannot be played without Mutant" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        gene <- putInHand p "38019"
        _ <- putInHand p "01090"
        offers (isPlay gene) `shouldReturn` False

    it "Moira does not trigger on hero to alter-ego form changes" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        asHero p
        moira <- putIntoPlay p "38018"
        runNow [IdentityMessage p $ ChangedToForm B]
        offers (cardAbility moira 1) `shouldReturn` False
        length <$> handOf p `shouldReturn` 0

    it "X-Gene cannot pay for an aspect event" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        asHero p
        gene <- putIntoPlay p "38019"
        event <- putInHand p "38016"
        _ <- putInHand p "01090"
        villainAttacksWithBoost p Minions.vulture
        choose "defend" isDefend
        choose "play aspect event" (isPlay event)
        offers (cardAbility gene 1) `shouldReturn` False

    it "Iceman spends all three counters over three separate entering minions" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        asHero p
        ice <- putIntoPlay p "38010"
        for_ [3, 2, 1 :: Natural] $ \remaining -> do
          mid <- minionOf <$> revealNow p Minions.hydraMercenary
          choose "freeze entering minion" (cardAbility ice 1)
          pass
          minionStunnedNow mid `shouldReturn` True
          getCountersOn (allyIdOf ice) `shouldReturn` (remaining - 1)
        a <- allyIn "38010"
        allyDamage a `shouldBe` 0

    it "Iceman cannot spend freeze counters to stun a stalwart minion" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        asHero p
        ice <- putIntoPlay p "38010"
        mid <- minionOf <$> revealNow p Minions.kreeLieutenant
        offers (cardAbility ice 1) `shouldReturn` False
        getCountersOn (allyIdOf ice) `shouldReturn` 3
        minionStunnedNow mid `shouldReturn` False

    it "Med Lab can hold a defeated controlled minion but cannot play its original minion card" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        asHero p
        lab <- putIntoPlay p "38028"
        mid <- addMinion p Minions.hydraMercenary
        karma <- putInHand p "38011"
        first <- putInHand p "01088"
        second <- putInHand p "01089"
        playWith karma [first, second]
        choose "Karma takes control" (cardAbility karma 1)
        choose "the mercenary" (isTargetChoice mid)
        let aid = AllyId $ unMinionId mid
        resolve [AllyMessage aid $ AllyDamaged (IdentityRef p) (toDamage 1 FromAbility)]
        host <- getsGame $ toAttrs . fromJust . HashMap.lookup aid . gameAllies
        choose "controlled minion attacks" (isAllyAttack host)
        chooseVillain
        choose "Med Lab captures the defeated controlled minion" (cardAbility lab 1)
        held <- concatMap supportHeldCards <$> supportsWith "38028"
        length held `shouldBe` 1
        pcOwner (testHead held) `shouldBe` Nothing
        cdCardType (getCardDef $ testHead held) `shouldBe` MinionType
        getEncounterDiscard >>= (`shouldSatisfy` all ((/= CardId (unMinionId mid)) . ecCardId))
        resolve [SupportMessage (supportIdOf lab) ReadiedSupport, IdentityMessage p $ ChangedToForm B]
        _ <- putInHand p "01088"
        _ <- putInHand p "01089"
        offers (cardAbility lab 2) `shouldReturn` False


  describe "Rogue timing regressions" do
    it "lets Moira draw a resource before playing a response to the same form change" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        clearHand p
        moira <- putIntoPlay p "38018"
        perseverance <- putInHand p "13033"
        top <- testHead <$> stackDeck p ["01090"]
        changeForm p
        choose "draw with Moira first" (cardAbility moira 1)
        offers (isPlay perseverance) `shouldReturn` True
        playWith perseverance [top]
        playerIdentityTough <$> playerAttrs p `shouldReturn` True

    it "offers generic and Mutant form responses in the same window" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        clearHand p
        moira <- putIntoPlay p "38018"
        perseverance <- putInHand p "13033"
        resource <- putInHand p "01090"
        changeForm p
        offers (cardAbility moira 1) `shouldReturn` True
        offers (isPlay perseverance) `shouldReturn` True
        playWith perseverance [resource]
        choose "Moira can still draw" (cardAbility moira 1)
        length <$> handOf p `shouldReturn` 1

    it "offers Med Lab alongside Rapid Response and stops offering capture after the ally returns to play" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        asHero p
        lab <- putIntoPlay p "38028"
        rapid <- putIntoPlay p "08031"
        ice <- putIntoPlay p "38010"
        resolve [AllyMessage (allyIdOf ice) $ AllyDamaged (IdentityRef p) (toDamage 2 FromAbility)]
        a <- allyIn "38010"
        choose "Iceman attacks" (isAllyAttack a)
        chooseVillain
        offers (cardAbility lab 1) `shouldReturn` True
        offers (cardAbility rapid 1) `shouldReturn` True
        windows <- getCurrentWindows
        windows `shouldSatisfy` any (\case W.Window _ (W.AllyCardDefeated _ _) -> True; _ -> False)
        windows `shouldSatisfy` any (\case W.Window _ (W.AllyCardDefeatedByConsequentialDamage _ _) -> True; _ -> False)
        choose "Rapid Response first" (cardAbility rapid 1)
        offers (cardAbility lab 1) `shouldReturn` False
        supportsWith "38028" >>= (`shouldSatisfy` all (not . supportExhausted))
        allyDamage <$> allyIn "38010" `shouldReturn` 1

    it "Med Lab's as-if-hand replay triggers White Tiger's hand-play response" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        asHero p
        lab <- putIntoPlay p "38028"
        tiger <- putIntoPlay p "21013"
        resolve [AllyMessage (allyIdOf tiger) $ AllyDamaged (IdentityRef p) (toDamage 1 FromAbility)]
        a <- allyIn "21013"
        choose "White Tiger attacks" (isAllyAttack a)
        chooseVillain
        choose "Med Lab holds White Tiger" (cardAbility lab 1)
        resolve [SupportMessage (supportIdOf lab) ReadiedSupport, IdentityMessage p $ ChangedToForm B]
        clearHand p
        energy <- putInHand p "01088"
        strength <- putInHand p "01090"
        top <- testHead <$> stackDeck p ["38005"]
        choose "Med Lab replay" (cardAbility lab 2)
        choose "White Tiger" (isTargetChoice tiger)
        payWith [energy, strength]
        choose "White Tiger counts as played from hand" (cardAbility tiger 1)
        handOf p `shouldReturn` [top]
        allyExhausted <$> allyIn "21013" `shouldReturn` True

    it "Med Lab's replay also triggers Karma's hand-play response" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        asHero p
        lab <- putIntoPlay p "38028"
        karma <- putIntoPlay p "38011"
        mid <- addMinion p Minions.hydraMercenary
        runNow [AllyMessage (allyIdOf karma) $ AllyDamaged (IdentityRef p) (toDamage 1 FromConsequential)]
        choose "Med Lab holds Karma" (cardAbility lab 1)
        resolve [SupportMessage (supportIdOf lab) ReadiedSupport, IdentityMessage p $ ChangedToForm B]
        first <- putInHand p "01088"
        second <- putInHand p "01089"
        choose "Med Lab replays Karma" (cardAbility lab 2)
        choose "Karma" (isTargetChoice karma)
        payWith [first, second]
        choose "Karma counts as played from hand" (cardAbility karma 1)
        choose "take control of the minion" (isTargetChoice mid)
        getsGame (HashMap.member mid . gameMinions) `shouldReturn` False
        allyExhausted <$> allyIn "38011" `shouldReturn` True

    it "Med Lab retrieves its consequentially defeated ally from the Collection" do
      tg <- testGameAgainst "16070" ["rogue"]
      let p = player1 tg
      inGame tg do
        withGame_ (collectionL .~ [])
        asHero p
        lab <- putIntoPlay p "38028"
        ice <- putIntoPlay p "38010"
        resolve [AllyMessage (allyIdOf ice) $ AllyDamaged (IdentityRef p) (toDamage 2 FromAbility)]
        a <- allyIn "38010"
        choose "Iceman attacks" (isAllyAttack a)
        chooseVillain
        vid <- villainIdNow
        choose "Collector's forced interrupt places Iceman in the Collection" (\case
          UseAbility ab -> abilitySource ab == VillainRef vid && abilityIndex ab == 1
          _ -> False)
        getCollection >>= (`shouldSatisfy` any ((== pcCardId ice) . toCardId))
        choose "Med Lab follows him into the Collection" (cardAbility lab 1)
        getCollection >>= (`shouldSatisfy` all ((/= pcCardId ice) . toCardId))
        held <- concatMap supportHeldCards <$> supportsWith "38028"
        held `shouldBe` [ice]

    it "Med Lab can retrieve the defeated physical card from the victory display" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        asHero p
        lab <- putIntoPlay p "38028"
        ice <- putIntoPlay p "38010"
        resolve [AllyMessage (allyIdOf ice) $ AllyDamaged (IdentityRef p) (toDamage 2 FromAbility)]
        a <- allyIn "38010"
        choose "Iceman attacks" (isAllyAttack a)
        chooseVillain
        -- A replacement destination is established while the same defeat window
        -- remains open, preserving the physical id and the consequential cause.
        withGame_ $ \g -> (g & entitiesL . playersL . ix p %~ \(PlayerIdentity attrs) -> PlayerIdentity attrs
          {playerIdentityDiscard = Discard $ filter ((/= pcCardId ice) . pcCardId) (unDiscard $ playerIdentityDiscard attrs)})
          & victoryDisplayL <>~ [MkEncounterCard (pcCardId ice) (getCardDef ice)]
        choose "Med Lab retrieves from victory" (cardAbility lab 1)
        getVictoryDisplay >>= (`shouldSatisfy` all ((/= pcCardId ice) . ecCardId))
        held <- concatMap supportHeldCards <$> supportsWith "38028"
        held `shouldBe` [ice]
