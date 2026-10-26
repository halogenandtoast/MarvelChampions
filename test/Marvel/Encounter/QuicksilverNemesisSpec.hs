module Marvel.Encounter.QuicksilverNemesisSpec (spec) where

import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Card.Side (Side (..))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Window qualified as W
import TestImport

quicksilver :: IO TestGame
quicksilver = testGame ["quicksilver"]

sideOf :: IdentityId -> GameAppT Side
sideOf p = (.playerIdentitySide) <$> playerAttrs p

-- | The player in hero form, past the change's responses.
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass

exhaustPietro, exhaustCannotReady, take2Indirect, exhaustIdentity, spendPhysicals :: Choice -> Bool
exhaustPietro = isLabel "$label.exhaustPietroMaximoffRemoveNeedForSpeed"
exhaustCannotReady = isLabel "$label.exhaustYourIdentityCannotReady"
take2Indirect = isLabel "$label.take2IndirectDamage"
exhaustIdentity = isLabel "$label.exhaustYourIdentity"
spendPhysicals = isLabel "$label.spendTwoPhysicalResources"

-- | Try to ready the player's identity, as a card or the ready step would.
tryReady :: IdentityId -> GameAppT ()
tryReady p = resolve [IdentityMessage p ReadiedIdentity]

-- | Earthquake turned up as the villain's boost card.
boostEarthquake :: GameAppT ()
boostEarthquake = do
  vid <- villainIdNow
  card <- encounterCard Treacheries.earthquake
  runNow [RevealBoostCard card (EnemyVillainId vid)]

spec :: Spec
spec = describe "Quicksilver nemesis" do
  describe "Need for Speed (14024)" do
    it "exhausting Pietro Maximoff removes it from the game" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Obligations.needForSpeed
        offers exhaustCannotReady `shouldReturn` True
        choose "exhaust Pietro" exhaustPietro
        isExhaustedNow p `shouldReturn` True
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "14024")
        -- nothing keeps him from readying
        tryReady p
        isExhaustedNow p `shouldReturn` False

    it "a hero may flip to alter-ego to exhaust Pietro" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- revealNow p Obligations.needForSpeed
        choose "flip to alter-ego" (isLabel "$label.changeToB")
        sideOf p `shouldReturn` B
        choose "exhaust Pietro" exhaustPietro
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "14024")

    it "otherwise the identity is exhausted and cannot ready until its next turn ends" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- revealNow p Obligations.needForSpeed
        choose "stay a hero" (isLabel "$label.doNotFlipToAlterEgo")
        -- a hero cannot exhaust Pietro: the other option resolves
        sideOf p `shouldReturn` A
        isExhaustedNow p `shouldReturn` True
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` elem "14024")
        tryReady p
        isExhaustedNow p `shouldReturn` True
        -- revealed during his turn: that turn's end is not the next turn's
        resolve [IdentityEndedTurn p]
        tryReady p
        isExhaustedNow p `shouldReturn` True
        resolve [CheckWindows [W.Window W.After (W.TurnBegan p)]]
        resolve [IdentityEndedTurn p]
        tryReady p
        isExhaustedNow p `shouldReturn` False

    it "an exhausted identity can choose neither option: it is only discarded" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        resolve [IdentityMessage p ExhaustedIdentity]
        _ <- revealNow p Obligations.needForSpeed
        offers exhaustPietro `shouldReturn` False
        offers exhaustCannotReady `shouldReturn` False
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` elem "14024")
        tryReady p
        isExhaustedNow p `shouldReturn` False

  describe "Extortion of Seismic Proportion (14025)" do
    it "incite 1, and 2 threat per player" do
      tg <- testGame ["quicksilver", "spider-man"]
      let p = player1 tg
      inGame tg do
        before <- mainSchemeThreatNow
        scheme <- sideSchemeOf <$> revealNow p SideSchemes.extortionOfSeismicProportion
        pass
        mainSchemeThreatNow `shouldReturn` (before + 1)
        sideSchemeThreat scheme `shouldReturn` 4

    it "crisis: threat cannot be removed from the main scheme while it is in play" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        clearHand p
        main <- mainSchemeIdNow
        scheme <- sideSchemeOf <$> revealNow p SideSchemes.extortionOfSeismicProportion
        pass
        threat <- mainSchemeThreatNow
        choose "thwart" (isHeroThwart p)
        offers (isTargetChoice main) `shouldReturn` False
        chooseIf (isTargetChoice scheme)
        pass
        mainSchemeThreatNow `shouldReturn` threat
        sideSchemeThreat scheme `shouldReturn` 1

  describe "Avalanche (14026)" do
    it "incite 2, and each player chooses 2 indirect damage or to exhaust their identity" do
      tg <- testGame ["quicksilver", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        before <- mainSchemeThreatNow
        card <- encounterCard Minions.avalanche
        runNow [RevealEncounterCard p card]
        fmap fst <$> asked `shouldReturn` Just p
        choose "take the damage" take2Indirect
        damageOf p `shouldReturn` 2
        isExhaustedNow p `shouldReturn` False
        fmap fst <$> asked `shouldReturn` Just other
        choose "exhaust" exhaustIdentity
        isExhaustedNow other `shouldReturn` True
        damageOf other `shouldReturn` 0
        mainSchemeThreatNow `shouldReturn` (before + 2)

    it "an exhausted identity cannot choose to exhaust: it takes the damage" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        resolve [IdentityMessage p ExhaustedIdentity]
        card <- encounterCard Minions.avalanche
        runNow [RevealEncounterCard p card]
        offers exhaustIdentity `shouldReturn` False
        damageOf p `shouldReturn` 2

  describe "Vibration Resistance (14027)" do
    it "attaches to Avalanche, if able: an attack on him deals 1 less" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        m <- addMinion p Minions.avalanche
        pass
        _ <- revealNow p Attachments.vibrationResistance
        pass
        choose "attack" (isHeroAttack p)
        choose "Avalanche" (isTargetChoice m)
        pass
        minionDamageOf m `shouldReturn` Just 1
        villainDamageNow `shouldReturn` 0

    it "attaches to the villain without Avalanche: attacks deal 1 less, other damage does not" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        clearHand p
        _ <- revealNow p Attachments.vibrationResistance
        pass
        -- Spider-Man's 2 ATK
        choose "attack" (isHeroAttack p)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 1
        -- Maria Hill's 1 ATK
        putIntoPlay p "01067"
        maria <- allyIn "01067"
        choose "attack with Maria Hill" (isAllyAttack maria)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 1
        -- Tac Team's 2 damage is no attack
        putIntoPlay p "01056"
        [team] <- supportsWith "01056"
        choose "use Tac Team" (isAbilityOf (supportId team))
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 3

    it "Hero Action: exhaust your hero to discard it" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        card <- revealNow p Attachments.vibrationResistance
        pass
        let attachment = AttachmentId $ unCardId $ ecCardId card
        -- not in alter-ego form
        offers (isAbilityOf attachment) `shouldReturn` False
        asHero p
        choose "discard it" (isAbilityOf attachment)
        pass
        isExhaustedNow p `shouldReturn` True
        encounterDiscardCodes >>= (`shouldSatisfy` elem "14027")
        -- gone: the attack is no longer reduced
        resolve [IdentityMessage p ReadiedIdentity]
        choose "attack" (isHeroAttack p)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 2

  describe "Earthquake (14028)" do
    it "incite 1: discard 2 cards from your hand and exhaust your identity" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        clearHand p
        c1 <- putInHand p "01088"
        c2 <- putInHand p "01089"
        c3 <- putInHand p "01090"
        before <- mainSchemeThreatNow
        _ <- revealNow p Treacheries.earthquake
        choose "discard" (isTargetChoice c1)
        choose "discard" (isTargetChoice c2)
        pass
        handOf p `shouldReturn` [c3]
        isExhaustedNow p `shouldReturn` True
        mainSchemeThreatNow `shouldReturn` (before + 1)

    it "[star] Boost: spend [physical] [physical] resources" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        clearHand p
        -- Strength: 2 [physical] resources
        strength <- putInHand p "01090"
        boostEarthquake
        offers exhaustIdentity `shouldReturn` True
        choose "spend" spendPhysicals
        choose "pay" (isPayWith strength)
        chooseIf isFinishPayment
        pass
        handOf p `shouldReturn` []
        isExhaustedNow p `shouldReturn` False

    it "[star] Boost: exhaust your identity when the resources cannot be spent" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        clearHand p
        -- Energy and Genius: no [physical]
        _ <- putInHand p "01088"
        _ <- putInHand p "01089"
        boostEarthquake
        offers spendPhysicals `shouldReturn` False
        isExhaustedNow p `shouldReturn` True
        length <$> handOf p `shouldReturn` 2

    it "[star] Boost: neither, exhausted and unable to pay" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        clearHand p
        resolve [IdentityMessage p ExhaustedIdentity]
        boostEarthquake
        offers exhaustIdentity `shouldReturn` False
        offers spendPhysicals `shouldReturn` False
        isExhaustedNow p `shouldReturn` True
