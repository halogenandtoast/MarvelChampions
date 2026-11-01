module Marvel.Hero.DraxSignatureSpec (spec) where

import Marvel.Count
import Marvel.Matchers qualified as M
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types (villainIsTough)
import TestImport

drax :: IO TestGame
drax = testGame ["drax"]

-- | Drax in hero form, his hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

-- | Vengeance counters on Drax: each is +1 ATK.
vengeance :: IdentityId -> Natural -> GameAppT ()
vengeance p n = resolve [PlaceCounters (toTarget p) n]

sideOf :: IdentityId -> GameAppT Side
sideOf p = (.playerIdentitySide) <$> playerAttrs p

-- | The villain attacks the player, a hydra mercenary (1 boost icon) on the encounter deck.
villainAttacks :: IdentityId -> GameAppT ()
villainAttacks p = do
  _ <- stackEncounterDeck [Minions.hydraMercenary]
  vid <- villainIdNow
  runNow [VillainMessage vid (VillainAttacks p)]

isAllyDefend :: AllyId -> Choice -> Bool
isAllyDefend a = \case
  AllyDefend a' _ -> a' == a
  _ -> False

exhaustAlterEgo, stunnedOption :: Choice -> Bool
exhaustAlterEgo = isLabel "$label.exhaustDraxRemoveMemoriesOfAnotherLife"
stunnedOption = isLabel "$label.stunnedOrSurgeDiscardObligation"

spec :: Spec
spec = describe "Drax's signature cards" do
  describe "\"Fight Me, Coward!\" (19003)" do
    it "readies your hero and draws 1 card, then the villain attacks you" do
      tg <- drax
      let p = player1 tg
      inGame tg do
        asHero p
        resolve [IdentityMessage p ExhaustedIdentity]
        fight <- putInHand p "19003"
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        playWith fight []
        chooseIf isFinishPayment
        isExhaustedNow p `shouldReturn` False
        length <$> handOf p `shouldReturn` 1
        -- readied, Drax may defend the attack
        offers isDefend `shouldReturn` True
        choose "take the attack" isNoDefender
        pass
        -- Rhino's 2 ATK and the boost icon
        damageOf p `shouldReturn` 3

    it "is a hero action" do
      tg <- drax
      let p = player1 tg
      inGame tg do
        clearHand p
        fight <- putInHand p "19003"
        offers (isPlay fight) `shouldReturn` False

  describe "Intimidation (19004)" do
    it "removes threat from a scheme equal to your ATK" do
      tg <- drax
      let p = player1 tg
      inGame tg do
        asHero p
        vengeance p 2
        placeMainThreat 5
        before <- mainSchemeThreatNow
        card <- putInHand p "19004"
        genius <- putInHand p "01089"
        playWith card [genius]
        chooseIf isFinishPayment
        chooseMainScheme
        -- 1 ATK and 2 vengeance counters
        mainSchemeThreatNow `shouldReturn` (before - 3)

  describe "Knife Leap (19005)" do
    it "costs 1 less for each vengeance counter, and gives +5 ATK to the basic attack" do
      tg <- drax
      let p = player1 tg
      inGame tg do
        asHero p
        vengeance p 3
        leap <- putInHand p "19005"
        choose "attack" (isHeroAttack p)
        choose "Knife Leap" (isPlay leap)
        chooseIf isFinishPayment
        pass
        vid <- villainIdNow
        choose "Rhino" (isTargetChoice vid)
        -- 1 ATK, 3 vengeance counters, 5 for Knife Leap
        villainDamageNow `shouldReturn` 9
        -- only for that attack
        selectCount M.HeroAttackDamage (M.IdentityWithId p) `shouldReturn` 4

    it "is not offered without the resources its reduced cost needs" do
      tg <- drax
      let p = player1 tg
      inGame tg do
        asHero p
        vengeance p 1
        leap <- putInHand p "19005"
        -- 1 resource of the 2 it costs
        _ <- putInHand p "19003"
        choose "attack" (isHeroAttack p)
        offers (isPlay leap) `shouldReturn` False

    it "that attack gains overkill" do
      tg <- drax
      let p = player1 tg
      inGame tg do
        asHero p
        vengeance p 2
        leap <- putInHand p "19005"
        fight <- putInHand p "19003"
        mercenary <- addMinion p Minions.hydraMercenary
        pass
        choose "attack" (isHeroAttack p)
        choose "Knife Leap" (isPlay leap)
        choose "pay" (isPayWith fight)
        chooseIf isFinishPayment
        pass
        choose "the mercenary" (isTargetChoice mercenary)
        minionDamageOf mercenary `shouldReturn` Nothing
        -- 8 damage to 3 hit points: 5 go on to Rhino
        villainDamageNow `shouldReturn` 5

    it "that attack gains piercing" do
      tg <- drax
      let p = player1 tg
      inGame tg do
        asHero p
        vengeance p 3
        vid <- villainIdNow
        resolve [VillainMessage vid VillainBecomeTough]
        leap <- putInHand p "19005"
        choose "attack" (isHeroAttack p)
        choose "Knife Leap" (isPlay leap)
        chooseIf isFinishPayment
        pass
        choose "Rhino" (isTargetChoice vid)
        villainIsTough <$> getVillain `shouldReturn` False
        villainDamageNow `shouldReturn` 9

  describe "Parry (19006)" do
    it "prevents damage equal to double your ATK" do
      tg <- drax
      let p = player1 tg
      inGame tg do
        asHero p
        vengeance p 1
        parry <- putInHand p "19006"
        vid <- villainIdNow
        start (damageIdentity p (toSource vid) (toDamage 5 FromAbility))
        playWith parry []
        chooseIf isFinishPayment
        pass
        -- 2 ATK: 4 prevented
        damageOf p `shouldReturn` 1

    it "is not offered in alter-ego form" do
      tg <- drax
      let p = player1 tg
      inGame tg do
        clearHand p
        parry <- putInHand p "19006"
        vid <- villainIdNow
        start (damageIdentity p (toSource vid) (toDamage 5 FromAbility))
        offers (isPlay parry) `shouldReturn` False

  describe "Payback (19007)" do
    it "after the villain attacks you, deals it damage equal to your ATK" do
      tg <- drax
      let p = player1 tg
      inGame tg do
        asHero p
        vengeance p 2
        payback <- putInHand p "19007"
        villainAttacks p
        choose "take the attack" isNoDefender
        choose "Payback" (isPlay payback)
        chooseIf isFinishPayment
        pass
        villainDamageNow `shouldReturn` 3

    -- reading: any villain attack against you, whichever of your characters defended
    it "is offered when your ally defended the villain's attack" do
      tg <- drax
      let p = player1 tg
      inGame tg do
        asHero p
        mantis <- putIntoPlay p "19002"
        payback <- putInHand p "19007"
        villainAttacks p
        choose "Mantis defends" (isAllyDefend (allyIdOf mantis))
        choose "Payback" (isPlay payback)
        chooseIf isFinishPayment
        pass
        villainDamageNow `shouldReturn` 1

    it "is not offered after a minion attacks" do
      tg <- drax
      let p = player1 tg
      inGame tg do
        asHero p
        payback <- putInHand p "19007"
        mercenary <- addMinion p Minions.hydraMercenary
        runNow [MinionMessage mercenary (MinionAttacks p)]
        choose "take the attack" isNoDefender
        offers (isPlay payback) `shouldReturn` False

  describe "Memories of Another Life (19025)" do
    it "is given to the Drax player" do
      tg <- testGame ["she-hulk", "drax"]
      let p1 = player1 tg
          p2 = player2 tg
      inGame tg do
        _ <- revealNow p1 Obligations.memoriesOfAnotherLife
        fmap fst <$> asked `shouldReturn` Just p2

    it "exhausting the alter-ego removes it from the game" do
      tg <- drax
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Obligations.memoriesOfAnotherLife
        offers stunnedOption `shouldReturn` True
        choose "exhaust Drax" exhaustAlterEgo
        isExhaustedNow p `shouldReturn` True
        isStunned p `shouldReturn` False
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "19025")

    it "a hero may flip to alter-ego to exhaust it" do
      tg <- drax
      let p = player1 tg
      inGame tg do
        changeForm p >> pass
        _ <- revealNow p Obligations.memoriesOfAnotherLife
        choose "flip to alter-ego" (isLabel "$label.changeToB")
        -- his alter-ego's Forced Response comes first
        chooseIf isAnyAbility
        sideOf p `shouldReturn` B
        choose "exhaust Drax" exhaustAlterEgo
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "19025")

    it "otherwise you are stunned, and it is discarded" do
      tg <- drax
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- revealNow p Obligations.memoriesOfAnotherLife
        chooseIf (isLabel "$label.doNotFlipToAlterEgo")
        offers exhaustAlterEgo `shouldReturn` False
        chooseIf stunnedOption
        pass
        isStunned p `shouldReturn` True
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` elem "19025")

    it "an exhausted alter-ego cannot be exhausted for it: stunned instead" do
      tg <- drax
      let p = player1 tg
      inGame tg do
        clearHand p
        resolve [IdentityMessage p ExhaustedIdentity]
        _ <- revealNow p Obligations.memoriesOfAnotherLife
        offers exhaustAlterEgo `shouldReturn` False
        chooseIf stunnedOption
        pass
        isStunned p `shouldReturn` True
        obligationsInPlay `shouldReturn` 0

    it "already stunned, it gains surge" do
      tg <- drax
      let p = player1 tg
      inGame tg do
        asHero p
        resolve [IdentityMessage p IdentityStunned]
        [next] <- stackEncounterDeck [Treacheries.advance]
        _ <- revealNow p Obligations.memoriesOfAnotherLife
        chooseIf (isLabel "$label.doNotFlipToAlterEgo")
        chooseIf stunnedOption
        pass
        isStunned p `shouldReturn` True
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` elem "19025")
        -- outside the villain phase the surge deals Advance to you
        dealtTo p >>= (`shouldSatisfy` (ecCardId next `elem`) . map ecCardId)

    it "not stunned, it does not surge" do
      tg <- drax
      let p = player1 tg
      inGame tg do
        asHero p
        [next] <- stackEncounterDeck [Treacheries.advance]
        _ <- revealNow p Obligations.memoriesOfAnotherLife
        chooseIf (isLabel "$label.doNotFlipToAlterEgo")
        chooseIf stunnedOption
        pass
        dealtTo p >>= (`shouldSatisfy` (ecCardId next `notElem`) . map ecCardId)
