module Marvel.Aspect.ScarletWitchPackSpec (spec) where

import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

scarletWitch :: IO TestGame
scarletWitch = testGame ["scarlet-witch"]

-- | Scarlet Witch in hero form, her hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = do
  changeForm p
  pass
  clearHand p

-- | Play a card from hand paying with new copies of the cards given.
playPaying :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
playPaying p card codes = do
  payment <- traverse (putInHand p) codes
  playWith card payment

spec :: Spec
spec = describe "The Scarlet Witch pack's aspect and basic cards" do
  describe "Speed (15010)" do
    it "readies after he thwarts, once per round" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        clearHand p
        placeMainThreat 6
        speed <- putIntoPlay p "15010"
        a <- allyIn "15010"
        choose "Speed thwarts" (isAllyThwart a)
        chooseMainScheme
        choose "ready Speed" (isAbilityOfCard speed)
        pass
        (.allyExhausted) <$> allyIn "15010" `shouldReturn` False
        choose "Speed thwarts again" (isAllyThwart a)
        chooseMainScheme
        offers (isAbilityOfCard speed) `shouldReturn` False
        pass
        (.allyExhausted) <$> allyIn "15010" `shouldReturn` True

  describe "Wiccan (15011)" do
    it "deals 1 damage to an enemy for each boost icon discarded, each point chosen" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        clearHand p
        placeMainThreat 6
        mercenary <- addMinion p Minions.hydraMercenary
        pass
        wiccan <- putIntoPlay p "15011"
        _ <- stackEncounterDeck [Treacheries.shadowOfThePast]
        a <- allyIn "15011"
        choose "Wiccan thwarts" (isAllyThwart a)
        chooseMainScheme
        choose "Wiccan's response" (isAbilityOfCard wiccan)
        pass
        choose "the mercenary" (isTargetChoice mercenary)
        pass
        chooseVillain
        pass
        minionDamageOf mercenary `shouldReturn` Just 1
        villainDamageNow `shouldReturn` 1
        map toCardCode <$> encounterDiscardNow >>= (`shouldSatisfy` elem (toCardCode Treacheries.shadowOfThePast))

    it "deals no damage for a card without boost icons" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        clearHand p
        placeMainThreat 6
        wiccan <- putIntoPlay p "15011"
        _ <- stackEncounterDeck [Treacheries.advance]
        a <- allyIn "15011"
        choose "Wiccan thwarts" (isAllyThwart a)
        chooseMainScheme
        choose "Wiccan's response" (isAbilityOfCard wiccan)
        pass
        vid <- villainIdNow
        offers (isTargetChoice vid) `shouldReturn` False
        villainDamageNow `shouldReturn` 0

  describe "Crisis Averted (15012)" do
    it "removes 6 threat from the main scheme" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 6
        before <- mainSchemeThreatNow
        crisis <- putInHand p "15012"
        playPaying p crisis ["01088", "01090"]
        pass
        mainSchemeThreatNow `shouldReturn` before - 6

    it "ignores a crisis icon when paid using a [mental] resource" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 6
        _ <- revealNow p SideSchemes.crowdControl
        pass
        before <- mainSchemeThreatNow
        crisis <- putInHand p "15012"
        playPaying p crisis ["01089", "01088"]
        pass
        mainSchemeThreatNow `shouldReturn` before - 6

    it "counts a wild resource declared as [mental]" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 6
        _ <- revealNow p SideSchemes.crowdControl
        pass
        before <- mainSchemeThreatNow
        crisis <- putInHand p "15012"
        wild <- putInHand p "13024"
        energy <- putInHand p "01088"
        playDeclaring crisis [wild, energy] Mental
        pass
        mainSchemeThreatNow `shouldReturn` before - 6

    it "removes no threat while a crisis icon is in play, paid without [mental]" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 6
        _ <- revealNow p SideSchemes.crowdControl
        pass
        before <- mainSchemeThreatNow
        crisis <- putInHand p "15012"
        playPaying p crisis ["01088", "01090"]
        pass
        mainSchemeThreatNow `shouldReturn` before

  describe "Multitasking (15013)" do
    it "removes 2 threat from a scheme" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 6
        scare <- sideSchemeOf <$> revealNow p SideSchemes.bombScare
        pass
        before <- mainSchemeThreatNow
        scareBefore <- sideSchemeThreat scare
        multi <- putInHand p "15013"
        playPaying p multi ["01090"]
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` before - 2
        sideSchemeThreat scare `shouldReturn` scareBefore

    it "removes 2 more from a different scheme when paid using a [mental] resource" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 6
        scare <- sideSchemeOf <$> revealNow p SideSchemes.bombScare
        pass
        before <- mainSchemeThreatNow
        scareBefore <- sideSchemeThreat scare
        multi <- putInHand p "15013"
        playPaying p multi ["01089"]
        chooseMainScheme
        m <- mainSchemeIdNow
        offers (isTargetChoice m) `shouldReturn` False
        choose "Bomb Scare" (isTargetChoice scare)
        pass
        mainSchemeThreatNow `shouldReturn` before - 2
        sideSchemeThreat scare `shouldReturn` scareBefore - 2

  describe "Swift Retribution (15014)" do
    it "makes the villain scheme, then deals 4 damage to him" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 2
        before <- mainSchemeThreatNow
        _ <- stackEncounterDeck [Treacheries.advance]
        swift <- putInHand p "15014"
        playPaying p swift ["01089"]
        pass
        chooseVillain
        pass
        -- Rhino I's SCH 1, a boost card without icons
        mainSchemeThreatNow `shouldReturn` before + 1
        villainDamageNow `shouldReturn` 4

  describe "Turn the Tide (15015)" do
    it "deals 3 damage after the hero's basic thwart clears a side scheme" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        crowd <- sideSchemeOf <$> revealNow p SideSchemes.crowdControl
        pass
        sideSchemeThreat crowd `shouldReturn` 2
        tide <- putInHand p "15015"
        choose "thwart" (isHeroThwart p)
        choose "Crowd Control" (isTargetChoice crowd)
        playWith tide []
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 3

    it "deals 3 damage after a thwart event clears the main scheme" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 2
        before <- mainSchemeThreatNow
        before `shouldSatisfy` (<= 6)
        tide <- putInHand p "15015"
        crisis <- putInHand p "15012"
        playPaying p crisis ["01088", "01090"]
        playWith tide []
        chooseVillain
        pass
        mainSchemeThreatNow `shouldReturn` 0
        villainDamageNow `shouldReturn` 3

    it "is not offered when threat remains on the scheme" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        scare <- sideSchemeOf <$> revealNow p SideSchemes.bombScare
        pass
        before <- sideSchemeThreat scare
        before `shouldSatisfy` (> 2)
        tide <- putInHand p "15015"
        choose "thwart" (isHeroThwart p)
        choose "Bomb Scare" (isTargetChoice scare)
        offers (isPlay tide) `shouldReturn` False
        sideSchemeThreat scare `shouldReturn` before - 2

    it "is not offered after an ally's thwart clears a scheme" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        crowd <- sideSchemeOf <$> revealNow p SideSchemes.crowdControl
        pass
        _ <- putIntoPlay p "01067"
        maria <- allyIn "01067"
        tide <- putInHand p "15015"
        choose "Maria Hill thwarts" (isAllyThwart maria)
        choose "Crowd Control" (isTargetChoice crowd)
        offers (isPlay tide) `shouldReturn` False

    it "is not offered after threat is removed without a thwart" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 2
        before <- mainSchemeThreatNow
        m <- mainSchemeIdNow
        tide <- putInHand p "15015"
        start [MainSchemeMessage m (MainSchemeThreatRemoved (toSource p) before)]
        offers (isPlay tide) `shouldReturn` False

  describe "Spiritual Meditation (15019)" do
    it "draws 2 cards, then discards 1 chosen from hand" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        clearHand p
        [c1, c2] <- stackDeck p ["01088", "01089"]
        meditation <- putInHand p "15019"
        playWith meditation []
        choose "discard Energy" (isTargetChoice c1)
        pass
        handOf p `shouldReturn` [c2]
        discardOf p >>= (`shouldSatisfy` elem c1)

    it "cannot be played without the Mystic trait" do
      tg <- testGame ["quicksilver"]
      let p = player1 tg
      inGame tg do
        clearHand p
        meditation <- putInHand p "15019"
        offers (isPlay meditation) `shouldReturn` False

  describe "Browbeat (15028)" do
    it "deals 2 damage to the villain and his stage number more" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        browbeat <- putInHand p "15028"
        playPaying p browbeat ["01090"]
        chooseVillain
        pass
        -- Rhino I: 2 + 1
        villainDamageNow `shouldReturn` 3

    it "cannot be played without the Avenger trait" do
      tg <- testGame ["ms-marvel"]
      let p = player1 tg
      inGame tg do
        changeForm p
        pass
        clearHand p
        browbeat <- putInHand p "15028"
        _ <- putInHand p "01090"
        offers (isPlay browbeat) `shouldReturn` False

  describe "Last Stand (15029)" do
    it "gives an attacking ally +3 ATK, then discards it after the attack" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        -- a Hero Interrupt: hero form
        asHero p
        _ <- putIntoPlay p "01067"
        maria <- allyIn "01067"
        stand <- putInHand p "15029"
        choose "Maria Hill attacks" (isAllyAttack maria)
        chooseVillain
        playWith stand []
        pass
        -- her ATK 1 and 3
        villainDamageNow `shouldReturn` 4
        alliesWith "01067" `shouldReturn` []
        map toCardCode <$> discardOf p >>= (`shouldSatisfy` \cs -> "01067" `elem` cs && "15029" `elem` cs)

    it "does nothing more when the ally is defeated by the attack's consequential damage" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        maria <- allyIdOf <$> putIntoPlay p "01067"
        resolve [AllyMessage maria (AllyDamaged GameRef (toDamage 1 FromAbility))]
        a <- allyIn "01067"
        stand <- putInHand p "15029"
        choose "Maria Hill attacks" (isAllyAttack a)
        chooseVillain
        playWith stand []
        pass
        villainDamageNow `shouldReturn` 4
        alliesWith "01067" `shouldReturn` []
        length . filter (== "01067") . map toCardCode <$> discardOf p `shouldReturn` 1

  describe "Bait and Switch (15030)" do
    it "has the villain attack you, then removes 4 threat from the main scheme" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 6
        before <- mainSchemeThreatNow
        _ <- stackEncounterDeck [Treacheries.advance]
        bait <- putInHand p "15030"
        playPaying p bait ["01088"]
        choose "take the attack" isNoDefender
        pass
        -- Rhino's ATK 2, a boost card without icons
        damageOf p `shouldReturn` 2
        mainSchemeThreatNow `shouldReturn` before - 4

  describe "Recuperation (15031)" do
    it "heals the alter-ego as much as her REC, without using her recovery" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        clearHand p
        setDamage p 5
        recuperation <- putInHand p "15031"
        playPaying p recuperation ["01090"]
        pass
        -- Wanda Maximoff's REC 3
        damageOf p `shouldReturn` 2
        exhaustedNow p `shouldReturn` False

    it "cannot be played with no damage to heal" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        clearHand p
        recuperation <- putInHand p "15031"
        _ <- putInHand p "01090"
        offers (isPlay recuperation) `shouldReturn` False

    it "cannot be played in hero form" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        setDamage p 5
        recuperation <- putInHand p "15031"
        _ <- putInHand p "01090"
        offers (isPlay recuperation) `shouldReturn` False
