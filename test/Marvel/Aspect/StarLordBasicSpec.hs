module Marvel.Aspect.StarLordBasicSpec (spec) where

import Marvel.Identity (getModifiedAllyLimit)
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Upgrade.Cards qualified as Upgrades
import TestImport

import Control.Monad (replicateM, replicateM_)

-- | Captain Marvel (neither Aerial nor Guardian) in hero form, her hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

-- | Captain Marvel in hero form made Aerial by Cosmic Flight.
asAerialHero :: IdentityId -> GameAppT ()
asAerialHero p = asHero p >> putIntoPlay p "01017" >> pure ()

-- | Play a card from hand paying with new Energy cards (2 resources each), declining an
-- interrupt to its being played.
playPaying :: IdentityId -> PlayerCard -> Int -> GameAppT ()
playPaying p card n = do
  payment <- replicateM n (putInHand p "01088")
  choose "play the card" (isPlay card)
  chooseIf isNoResponse
  payWith payment

cosmoDamage :: GameAppT Natural
cosmoDamage = (.allyDamage) <$> allyIn "17020"

spec :: Spec
spec = describe "Star-Lord pack basic and other aspect cards" do
  describe "Cosmo (17020)" do
    it "takes no consequential damage from an attack when the named type is discarded" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putIntoPlay p "17020"
        _ <- stackEncounterDeck [Treacheries.shadowOfThePast]
        cosmo <- allyIn "17020"
        choose "Cosmo attacks" (isAllyAttack cosmo)
        chooseVillain
        choose "Cosmo's interrupt" (isAbilityOfCard card)
        choose "name treachery" (isLabel "$label.nameTreacheryType")
        choose "the encounter deck" (isLabel "$label.encounterDeck")
        pass
        villainDamageNow `shouldReturn` 1
        cosmoDamage `shouldReturn` 0
        map toCardCode <$> encounterDiscardNow >>= (`shouldSatisfy` elem (toCardCode Treacheries.shadowOfThePast))

    it "takes it when the card is of another type" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putIntoPlay p "17020"
        _ <- stackEncounterDeck [Treacheries.shadowOfThePast]
        cosmo <- allyIn "17020"
        choose "Cosmo attacks" (isAllyAttack cosmo)
        chooseVillain
        choose "Cosmo's interrupt" (isAbilityOfCard card)
        choose "name minion" (isLabel "$label.nameMinionType")
        choose "the encounter deck" (isLabel "$label.encounterDeck")
        pass
        cosmoDamage `shouldReturn` 1

    it "thwarts discarding from a player deck, and only that use is spared" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        placeMainThreat 6
        card <- putIntoPlay p "17020"
        stacked <- stackDeck p ["01088"]
        cosmo <- allyIn "17020"
        before <- mainSchemeThreatNow
        choose "Cosmo thwarts" (isAllyThwart cosmo)
        chooseMainScheme
        choose "Cosmo's interrupt" (isAbilityOfCard card)
        choose "name resource" (isLabel "$label.nameResourceType")
        choose "Captain Marvel's deck" (isTargetChoice p)
        pass
        mainSchemeThreatNow `shouldReturn` before - 1
        cosmoDamage `shouldReturn` 0
        discarded <- discardOf p
        stacked `shouldSatisfy` all (`elem` discarded)
        -- readied, he attacks without the interrupt: consequential damage again
        runNow [AllyMessage (allyId cosmo) ReadiedAlly]
        choose "Cosmo attacks" (isAllyAttack cosmo)
        chooseVillain
        pass
        cosmoDamage `shouldReturn` 1

  describe "C.I.T.T. (17021)" do
    it "exhausts and spends 2 resources to ready an exhausted Guardian character" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 6
        citt <- putIntoPlay p "17021"
        _ <- putIntoPlay p "17020"
        cosmo <- allyIn "17020"
        choose "Cosmo thwarts" (isAllyThwart cosmo)
        chooseMainScheme
        pass
        (.allyExhausted) <$> allyIn "17020" `shouldReturn` True
        energy <- putInHand p "01088"
        choose "C.I.T.T." (isAbilityOfCard citt)
        payWith [energy]
        chooseIf (isTargetChoice (allyId cosmo))
        (.allyExhausted) <$> allyIn "17020" `shouldReturn` False
        supportsWith "17021" >>= (`shouldSatisfy` all (.supportExhausted))

    it "is not offered with no exhausted Guardian character" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        citt <- putIntoPlay p "17021"
        _ <- putIntoPlay p "17020"
        _ <- putInHand p "01088"
        offers (isAbilityOfCard citt) `shouldReturn` False

  describe "Knowhere (17022)" do
    it "can't be played without a Guardian identity" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        knowhere <- putInHand p "17022"
        _ <- putInHand p "01088"
        offers (isPlay knowhere) `shouldReturn` False

    it "raises the ally limit by 1" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        before <- getModifiedAllyLimit =<< getPlayer p
        _ <- putIntoPlay p "17022"
        (getModifiedAllyLimit =<< getPlayer p) `shouldReturn` before + 1

    it "after a player plays a Guardian ally, exhausts to have them draw 1" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        knowhere <- putIntoPlay p "17022"
        cosmo <- putInHand p "17020"
        playPaying p cosmo 1
        choose "Knowhere" (isAbilityOfCard knowhere)
        length <$> handOf p `shouldReturn` 1
        supportsWith "17022" >>= (`shouldSatisfy` all (.supportExhausted))

    it "is not offered for an ally that is not a Guardian" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        knowhere <- putIntoPlay p "17022"
        hawkeye <- putInHand p "01066"
        playPaying p hawkeye 2
        offers (isAbilityOfCard knowhere) `shouldReturn` False

    it "counts an ally Star-Lord makes a Guardian (FAQ)" do
      tg <- testGame ["star-lord"]
      let p = player1 tg
      inGame tg do
        asHero p
        knowhere <- putIntoPlay p "17022"
        hawkeye <- putInHand p "01066"
        playPaying p hawkeye 2
        choose "Knowhere" (isAbilityOfCard knowhere)
        length <$> handOf p `shouldReturn` 1

  describe "Pulse Grenade (17023)" do
    it "is discarded to deal the chosen enemy 1 damage per boost icon of the top 2 encounter cards" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        grenade <- putIntoPlay p "17023"
        -- 2 boost icons, then none
        _ <- stackEncounterDeck [Treacheries.shadowOfThePast, Treacheries.advance]
        choose "Pulse Grenade" (isAbilityOfCard grenade)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 2
        upgradeCodes >>= (`shouldSatisfy` notElem "17023")
        codes <- map toCardCode <$> encounterDiscardNow
        codes `shouldSatisfy` elem (toCardCode Treacheries.shadowOfThePast)
        codes `shouldSatisfy` elem (toCardCode Treacheries.advance)

    it "attacks with no damage for cards without boost icons: retaliate answers it" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        whiplash <- addMinion p Minions.whiplash
        pass
        grenade <- putIntoPlay p "17023"
        before <- damageOf p
        _ <- stackEncounterDeck [Treacheries.advance, Treacheries.advance]
        choose "Pulse Grenade" (isAbilityOfCard grenade)
        choose "Whiplash" (isTargetChoice whiplash)
        pass
        minionDamageOf whiplash `shouldReturn` Just 0
        damageOf p `shouldReturn` before + 1

  describe "Dive Bomb (17028)" do
    it "can't be played without the Aerial trait" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        dive <- putInHand p "17028"
        _ <- replicateM 2 (putInHand p "01088")
        offers (isPlay dive) `shouldReturn` False

    it "deals 7 damage to an enemy, then 1 to each other enemy" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asAerialHero p
        shocker <- addMinion p Minions.shocker
        pass
        dive <- putInHand p "17028"
        playPaying p dive 2
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 7
        minionDamageOf shocker `shouldReturn` Just 1

    it "a guard minion it defeats first leaves the villain open to the 1 damage (FAQ)" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asAerialHero p
        merc <- addMinion p Minions.hydraMercenary
        shocker <- addMinion p Minions.shocker
        pass
        dive <- putInHand p "17028"
        playPaying p dive 2
        choose "the Hydra Mercenary" (isTargetChoice merc)
        pass
        minionDamageOf merc `shouldReturn` Nothing
        minionDamageOf shocker `shouldReturn` Just 1
        villainDamageNow `shouldReturn` 1

    it "a guard minion still in play keeps the villain from the 1 damage (FAQ)" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asAerialHero p
        merc <- addMinion p Minions.hydraMercenary
        shocker <- addMinion p Minions.shocker
        pass
        dive <- putInHand p "17028"
        playPaying p dive 2
        choose "Shocker" (isTargetChoice shocker)
        pass
        minionDamageOf shocker `shouldReturn` Nothing
        minionDamageOf merc `shouldReturn` Just 1
        villainDamageNow `shouldReturn` 0

  describe "Agile Flight (17029)" do
    it "removes a total of 5 threat divided among schemes" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asAerialHero p
        scheme <- sideSchemeOf <$> revealNow p SideSchemes.bombScare
        pass
        placeMainThreat 6
        sideBefore <- sideSchemeThreat scheme
        mainBefore <- mainSchemeThreatNow
        flight <- putInHand p "17029"
        playPaying p flight 2
        m <- mainSchemeIdNow
        replicateM_ 4 $ choose "the main scheme" (isTargetChoice m)
        choose "the side scheme" (isTargetChoice (SchemeSideSchemeId scheme))
        pass
        mainSchemeThreatNow `shouldReturn` mainBefore - 4
        sideSchemeThreat scheme `shouldReturn` sideBefore - 1

    it "may stop short of 5" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asAerialHero p
        scheme <- sideSchemeOf <$> revealNow p SideSchemes.bombScare
        pass
        placeMainThreat 6
        sideBefore <- sideSchemeThreat scheme
        mainBefore <- mainSchemeThreatNow
        flight <- putInHand p "17029"
        playPaying p flight 2
        m <- mainSchemeIdNow
        replicateM_ 2 $ choose "the main scheme" (isTargetChoice m)
        choose "stop" (isLabel "$label.done")
        pass
        mainSchemeThreatNow `shouldReturn` mainBefore - 2
        sideSchemeThreat scheme `shouldReturn` sideBefore

    it "can't be played without the Aerial trait" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 6
        flight <- putInHand p "17029"
        _ <- replicateM 2 (putInHand p "01088")
        offers (isPlay flight) `shouldReturn` False

  describe "Ever Vigilant (17030)" do
    it "readies your hero and removes 2 threat from the main scheme" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asAerialHero p
        placeMainThreat 5
        choose "Captain Marvel thwarts" (isHeroThwart p)
        chooseMainScheme
        pass
        exhaustedNow p `shouldReturn` True
        before <- mainSchemeThreatNow
        vigilant <- putInHand p "17030"
        playPaying p vigilant 1
        m <- mainSchemeIdNow
        chooseIf (isTargetChoice m)
        pass
        exhaustedNow p `shouldReturn` False
        mainSchemeThreatNow `shouldReturn` before - 2

    it "removes no threat while a crisis icon is in play" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asAerialHero p
        placeMainThreat 5
        _ <- revealNow p SideSchemes.crowdControl
        pass
        before <- mainSchemeThreatNow
        vigilant <- putInHand p "17030"
        playPaying p vigilant 1
        m <- mainSchemeIdNow
        chooseIf (isTargetChoice m)
        pass
        mainSchemeThreatNow `shouldReturn` before

  describe "Enhanced Awareness (17031)" do
    it "plays as 03034: a [mental] resource from a use, its own printing" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        awareness <- putIntoPlay p "17031"
        upgradesWith "17031" >>= \case
          [u] -> do
            u.upgradeUses `shouldBe` 3
            sameCard (upgradeCardDef u) Upgrades.enhancedAwareness `shouldBe` True
          _ -> error "no Enhanced Awareness"
        cosmo <- putInHand p "17020"
        energy <- putInHand p "01088"
        choose "play Cosmo" (isPlay cosmo)
        choose "pay with awareness" (isAbilityOfCard awareness)
        payWith [energy]
        _ <- allyIn "17020"
        upgradesWith "17031" >>= \case
          [u] -> u.upgradeUses `shouldBe` 2
          _ -> error "no Enhanced Awareness"
