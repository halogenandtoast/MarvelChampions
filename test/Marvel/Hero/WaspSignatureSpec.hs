module Marvel.Hero.WaspSignatureSpec (spec) where

import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.SideScheme.Cards qualified as SideSchemes
import TestImport

wasp :: IO TestGame
wasp = testGame ["wasp"]

-- | The side of Wasp's card showing: A Tiny, B Nadia Van Dyne, C Giant.
sideOf :: IdentityId -> GameAppT Side
sideOf p = (.playerIdentitySide) <$> playerAttrs p

-- | Wasp in the form given, her hand empty and her turn's form change spent.
inForm :: IdentityId -> Side -> GameAppT ()
inForm p side = do
  changeFormTo p side
  pass
  clearHand p

-- | Play a card from hand paying with new copies of the cards given.
playPaying :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
playPaying p card codes = do
  payment <- traverse (putInHand p) codes
  playWith card payment

isChangeForm :: IdentityId -> Choice -> Bool
isChangeForm p = \case
  UseAbility ab -> ChangeForm `elem` abilityChoices ab && abilitySource ab == toSource p
  _ -> False

-- | Rapid Growth and a Genius to pay for it put in hand, before the basic power is
-- used, then Rapid Growth played as the interrupt to it.
withRapidGrowth :: IdentityId -> GameAppT () -> GameAppT ()
withRapidGrowth p usePower = do
  growth <- putInHand p "13005"
  genius <- putInHand p "01089"
  usePower
  playWith growth [genius]
  pass

-- | Rhino attacks, a Sonic Converter (3 boost icons) his boost card: 5 damage.
rhinoAttacks :: IdentityId -> GameAppT ()
rhinoAttacks p = do
  _ <- stackEncounterDeck [Attachments.sonicConverter]
  vid <- villainIdNow
  start [VillainMessage vid (VillainAttacks p)]

spec :: Spec
spec = describe "Wasp signature" do
  describe "Giant Help (13003)" do
    it "removes 3 threat from a scheme" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p A
        placeMainThreat 6
        before <- mainSchemeThreatNow
        help <- putInHand p "13003"
        playPaying p help ["01089", "01089"]
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` before - 3

    it "in Giant form removes a total of 4 divided among schemes instead" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p C
        scheme <- sideSchemeOf <$> revealNow p SideSchemes.bombScare
        pass
        placeMainThreat 6
        let side = SchemeSideSchemeId scheme
        sideBefore <- sideSchemeThreat scheme
        mainBefore <- mainSchemeThreatNow
        help <- putInHand p "13003"
        playPaying p help ["01089", "01089"]
        m <- mainSchemeIdNow
        choose "the side scheme" (isTargetChoice side)
        choose "the main scheme" (isTargetChoice m)
        choose "the main scheme" (isTargetChoice m)
        choose "the main scheme" (isTargetChoice m)
        pass
        sideSchemeThreat scheme `shouldReturn` sideBefore - 1
        mainSchemeThreatNow `shouldReturn` mainBefore - 3

  describe "Pinpoint Strike (13004)" do
    it "deals 7 damage to an enemy" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p C
        strike <- putInHand p "13004"
        playPaying p strike ["01089", "01089", "01089"]
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 7

    it "has no overkill outside Tiny form" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p C
        shocker <- addMinion p Minions.shocker
        pass
        strike <- putInHand p "13004"
        playPaying p strike ["01089", "01089", "01089"]
        choose "Shocker" (isTargetChoice shocker)
        pass
        minionDamageOf shocker `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 0

    it "in Tiny form deals 8 damage" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p A
        strike <- putInHand p "13004"
        playPaying p strike ["01089", "01089", "01089"]
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 8

    it "in Tiny form gains overkill: the excess over a minion's hit points goes to the villain" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p A
        shocker <- addMinion p Minions.shocker
        pass
        strike <- putInHand p "13004"
        playPaying p strike ["01089", "01089", "01089"]
        choose "Shocker" (isTargetChoice shocker)
        -- Small but Mighty declined: only the overkill reaches Rhino
        pass
        minionDamageOf shocker `shouldReturn` Nothing
        -- Shocker has 3 hit points
        villainDamageNow `shouldReturn` 5

  describe "Rapid Growth (13005)" do
    it "is not played but as a basic power is used" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p A
        growth <- putInHand p "13005"
        _ <- putInHand p "01089"
        offers (isPlay growth) `shouldReturn` False

    it "a basic attack from Tiny form: changes to Giant, whose ATK gets +2" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p A
        withRapidGrowth p $ choose "attack" (isHeroAttack p)
        chooseVillain
        pass
        sideOf p `shouldReturn` C
        -- Giant ATK 2, not Tiny's 1
        villainDamageNow `shouldReturn` 4

    it "a basic attack in Giant form: no change, still +2" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p C
        withRapidGrowth p $ choose "attack" (isHeroAttack p)
        chooseVillain
        pass
        sideOf p `shouldReturn` C
        villainDamageNow `shouldReturn` 4

    it "a basic thwart from Tiny form: changes to Giant, whose THW gets +2" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p A
        placeMainThreat 6
        before <- mainSchemeThreatNow
        withRapidGrowth p $ choose "thwart" (isHeroThwart p)
        chooseMainScheme
        pass
        sideOf p `shouldReturn` C
        -- Giant THW 2, not Tiny's 1
        mainSchemeThreatNow `shouldReturn` before - 4

    it "a basic thwart in Giant form: no change, still +2" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p C
        placeMainThreat 6
        before <- mainSchemeThreatNow
        withRapidGrowth p $ choose "thwart" (isHeroThwart p)
        chooseMainScheme
        pass
        sideOf p `shouldReturn` C
        mainSchemeThreatNow `shouldReturn` before - 4

    it "a defense from Tiny form: changes to Giant, whose DEF gets +2" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p A
        withRapidGrowth p do
          rhinoAttacks p
          choose "defend" isDefend
        resolve []
        sideOf p `shouldReturn` C
        -- Giant DEF 3 + 2 against 5; Tiny's 2 + 2 would have let 1 through
        damageOf p `shouldReturn` 0

    it "a defense in Giant form: no change, still +2" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p C
        withRapidGrowth p do
          rhinoAttacks p
          choose "defend" isDefend
        resolve []
        sideOf p `shouldReturn` C
        damageOf p `shouldReturn` 0

    it "its change of form is not the turn's own change of form" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p A
        -- a villain phase that brings no minion to guard Rhino
        _ <- stackEncounterDeck [SideSchemes.bombScare, SideSchemes.bombScare]
        endTurn
        pass
        clearHand p
        offers (isChangeForm p) `shouldReturn` True
        withRapidGrowth p $ choose "attack" (isHeroAttack p)
        chooseVillain
        pass
        sideOf p `shouldReturn` C
        offers (isChangeForm p) `shouldReturn` True

  describe "Wasp Sting (13006)" do
    -- a Hero Action: as a hero she is always Tiny or Giant, so its form criteria never
    -- decide it on their own
    it "is not played as Nadia Van Dyne" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        clearHand p
        sting <- putInHand p "13006"
        _ <- traverse (putInHand p) ["01089", "01089"]
        offers (isPlay sting) `shouldReturn` False

    it "in Giant form deals a total of 4 damage divided among enemies" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p C
        shocker <- addMinion p Minions.shocker
        pass
        v <- villainIdNow
        sting <- putInHand p "13006"
        playPaying p sting ["01089", "01089"]
        choose "the villain" (isTargetChoice v)
        choose "the villain" (isTargetChoice v)
        choose "the villain" (isTargetChoice v)
        choose "Shocker" (isTargetChoice shocker)
        pass
        villainDamageNow `shouldReturn` 3
        minionDamageOf shocker `shouldReturn` Just 1

    it "in Tiny form deals 5 damage to an enemy" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p A
        _ <- addMinion p Minions.shocker
        pass
        sting <- putInHand p "13006"
        playPaying p sting ["01089", "01089"]
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 5

  describe "Red Dreams (13026)" do
    it "exhausting Nadia Van Dyne removes it from the game" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Obligations.redDreams
        choose "exhaust Nadia" (isLabel "$label.exhaustNadiaVanDyneRemoveRedDreams")
        isExhaustedNow p `shouldReturn` True
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "13026")

    it "a hero may flip to alter-ego, and then exhaust Nadia" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p C
        _ <- revealNow p Obligations.redDreams
        choose "flip to alter-ego" (isLabel "$label.changeToB")
        sideOf p `shouldReturn` B
        choose "exhaust Nadia" (isLabel "$label.exhaustNadiaVanDyneRemoveRedDreams")
        isExhaustedNow p `shouldReturn` True
        obligationsInPlay `shouldReturn` 0

    it "otherwise each card with a printed [mental] resource is discarded from hand and she takes 1 damage" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p A
        genius <- putInHand p "01089"
        firstAid <- putInHand p "01086"
        strength <- putInHand p "01090"
        _ <- revealNow p Obligations.redDreams
        choose "stay a hero" (isLabel "$label.doNotFlipToAlterEgo")
        -- a hero cannot exhaust Nadia: the other choice is made
        pass
        discardOf p >>= (`shouldSatisfy` \d -> genius `elem` d && firstAid `elem` d)
        handOf p `shouldReturn` [strength]
        damageOf p `shouldReturn` 1
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` elem "13026")

    it "an exhausted Nadia Van Dyne cannot be exhausted for it" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        clearHand p
        resolve [IdentityMessage p ExhaustedIdentity]
        _ <- revealNow p Obligations.redDreams
        pass
        damageOf p `shouldReturn` 1
        encounterDiscardCodes >>= (`shouldSatisfy` elem "13026")
