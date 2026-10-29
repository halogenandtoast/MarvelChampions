module Marvel.Hero.HulkSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Count
import Marvel.Matchers qualified as M
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import TestImport

hulk :: IO TestGame
hulk = testGame ["hulk"]

-- | Change to Hulk, and keep a hand of just the cards given.
asHulk :: IdentityId -> [CardCode] -> GameAppT [PlayerCard]
asHulk p codes = do
  changeForm p
  pass
  clearHand p
  traverse (putInHand p) codes

isRecover :: Choice -> Bool
isRecover = \case
  UseAbility ab -> Recover `elem` abilityChoices ab
  _ -> False

isFormLabel :: Choice -> Bool
isFormLabel = \case
  ChangeToForm _ -> True
  _ -> False

spec :: Spec
spec = describe "Hulk" do
  describe "Hulk (10001a)" do
    it "Enraged: his hand is discarded as his turn ends, then drawn back up to 4" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        cards <- asHulk p ["01088", "01089", "10002"]
        choose "end the turn" isEndTurn
        choose "Enraged" (isNamedAbility "Enraged")
        discard <- discardOf p
        for_ cards \c -> discard `shouldSatisfy` elem c
        keepCards
        length <$> handOf p `shouldReturn` 4
        hand <- handOf p
        for_ cards \c -> hand `shouldSatisfy` notElem c

    it "Enraged does not answer Bruce Banner's turn ending" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        clearHand p
        energy <- putInHand p "01088"
        choose "end the turn" isEndTurn
        offers (isNamedAbility "Enraged") `shouldReturn` False
        keepCards
        handOf p >>= (`shouldSatisfy` elem energy)

    it "has 0 THW, 3 ATK and 18 hit points" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        _ <- asHulk p []
        hitPointsOf p `shouldReturn` 18
        handSizeOf p `shouldReturn` 4
        choose "attack" (isHeroAttack p)
        vid <- villainIdNow
        choose "attack Rhino" (isTargetChoice vid)
        villainDamageNow `shouldReturn` 3

  describe "Bruce Banner (10001b)" do
    it "Experimental Research: draw 1 card and discard 1, once a round" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        clearHand p
        energy <- putInHand p "01088"
        [genius] <- stackDeck p ["01089"]
        choose "Experimental Research" (isNamedAbility "Experimental Research")
        choose "discard Energy" (isTargetChoice energy)
        map pcCardId <$> handOf p `shouldReturn` [pcCardId genius]
        discardOf p >>= (`shouldSatisfy` elem energy)
        offers (isNamedAbility "Experimental Research") `shouldReturn` False

    it "recovers 4" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        setDamage p 10
        choose "recover" isRecover
        damageOf p `shouldReturn` 6

  describe "Crushing Blow (10002)" do
    it "deals damage equal to his ATK, paid with only [physical] resources" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [blow, strength, energy] <- asHulk p ["10002", "01090", "01088"]
        choose "play Crushing Blow" (isPlay blow)
        offers (isPayWith energy) `shouldReturn` False
        choose "pay with Strength" (isPayWith strength)
        chooseIf isFinishPayment
        vid <- villainIdNow
        choose "Rhino" (isTargetChoice vid)
        villainDamageNow `shouldReturn` 3
        handOf p `shouldReturn` [energy]

    it "cannot be played with no [physical] resource to pay with" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [blow, _] <- asHulk p ["10002", "01088"]
        offers (isPlay blow) `shouldReturn` False

    it "costs nothing once its cost is reduced to 0, with no [physical] resource spent (FAQ)" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [blow] <- asHulk p ["10002"]
        heli <- putIntoPlay p "01092"
        choose "use Helicarrier" (isAbilityOfCard heli)
        choose "play Crushing Blow" (isPlay blow)
        vid <- villainIdNow
        choose "Rhino" (isTargetChoice vid)
        villainDamageNow `shouldReturn` 3

  describe "Hulk Smash (10003)" do
    it "gives +10 ATK to the basic attack it answers" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [smash, energy, genius] <- asHulk p ["10003", "01088", "01089"]
        _ <- putInHand p "01088"
        choose "attack" (isHeroAttack p)
        choose "Hulk Smash" (isPlay smash)
        choose "pay" (isPayWith energy)
        choose "pay" (isPayWith genius)
        chooseIf isFinishPayment
        pass
        vid <- villainIdNow
        choose "Rhino" (isTargetChoice vid)
        villainDamageNow `shouldReturn` 13
        -- only for that attack
        selectCount M.HeroAttackDamage (M.IdentityWithId p) `shouldReturn` 3

    it "gains overkill when paid with only [physical] resources" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [smash, strength, _] <- asHulk p ["10003", "01090", "01090"]
        mercenary <- addMinion p Minions.hydraMercenary
        pass
        choose "attack" (isHeroAttack p)
        choose "Hulk Smash" (isPlay smash)
        choose "pay" (isPayWith strength)
        whenM (offers isPayWith') $ choose "pay" isPayWith'
        chooseIf isFinishPayment
        pass
        choose "the mercenary" (isTargetChoice mercenary)
        minionDamageOf mercenary `shouldReturn` Nothing
        -- 13 damage to 3 hit points: 10 go on to Rhino
        villainDamageNow `shouldReturn` 10

    it "has no overkill when paid otherwise" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [smash, strength, energy] <- asHulk p ["10003", "01090", "01088"]
        mercenary <- addMinion p Minions.hydraMercenary
        pass
        choose "attack" (isHeroAttack p)
        choose "Hulk Smash" (isPlay smash)
        choose "pay" (isPayWith strength)
        choose "pay" (isPayWith energy)
        chooseIf isFinishPayment
        pass
        choose "the mercenary" (isTargetChoice mercenary)
        minionDamageOf mercenary `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 0

  describe "Sub-Orbital Leap (10004)" do
    it "removes 5 threat paid with only [physical] resources" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [leap, strength, _] <- asHulk p ["10004", "01090", "10007"]
        placeMainThreat 6
        before <- mainSchemeThreatNow
        playWith leap [strength]
        whenM (offers isPayWith') $ choose "pay" isPayWith'
        chooseIf isFinishPayment
        chooseMainScheme
        mainSchemeThreatNow `shouldReturn` (before - 5)

    it "removes 3 otherwise" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [leap, strength, genius] <- asHulk p ["10004", "01090", "01089"]
        placeMainThreat 6
        before <- mainSchemeThreatNow
        playWith leap [strength, genius]
        chooseMainScheme
        mainSchemeThreatNow `shouldReturn` (before - 3)

  describe "Thunderclap (10005)" do
    it "deals 3 damage to each of up to 3 different enemies" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [clap, strength, genius] <- asHulk p ["10005", "01090", "01089"]
        mercenary <- addMinion p Minions.hydraMercenary
        pass
        playWith clap [strength, genius]
        choose "the mercenary" (isTargetChoice mercenary)
        -- the same enemy is not chosen twice
        offers (isTargetChoice mercenary) `shouldReturn` False
        vid <- villainIdNow
        choose "Rhino" (isTargetChoice vid)
        minionDamageOf mercenary `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 3

    it "deals its damage to all of them at once, before any is defeated" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [clap, strength, genius] <- asHulk p ["10005", "01090", "01089"]
        putIntoPlay p "01063"
        first <- addMinion p Minions.hydraMercenary
        second <- addMinion p Minions.hydraMercenary
        pass
        playWith clap [strength, genius]
        choose "the first mercenary" (isTargetChoice first)
        choose "the second mercenary" (isTargetChoice second)
        vid <- villainIdNow
        choose "Rhino" (isTargetChoice vid)
        -- Interrogation Room answers the first defeat: the second mercenary has taken
        -- its damage already
        [room] <- supportsWith "01063"
        offers (isAbilityOf (supportId room)) `shouldReturn` True
        minionDamageOf first `shouldReturn` Nothing
        minionDamageOf second `shouldReturn` Just 3
        villainDamageNow `shouldReturn` 3

    it "may stop at fewer" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [clap, strength, genius] <- asHulk p ["10005", "01090", "01089"]
        mercenary <- addMinion p Minions.hydraMercenary
        pass
        playWith clap [strength, genius]
        choose "the mercenary" (isTargetChoice mercenary)
        choose "done" (isLabel "$label.done")
        minionDamageOf mercenary `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 0

  describe "Unstoppable Force (10006)" do
    it "readies Hulk and draws a card when paid with only [physical] resources" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [force, strength] <- asHulk p ["10006", "01090"]
        choose "attack" (isHeroAttack p)
        vid <- villainIdNow
        choose "Rhino" (isTargetChoice vid)
        isExhaustedNow p `shouldReturn` True
        playWith force [strength]
        isExhaustedNow p `shouldReturn` False
        length <$> handOf p `shouldReturn` 1

    it "draws nothing otherwise" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [force, genius] <- asHulk p ["10006", "01089"]
        choose "attack" (isHeroAttack p)
        vid <- villainIdNow
        choose "Rhino" (isTargetChoice vid)
        playWith force [genius]
        isExhaustedNow p `shouldReturn` False
        length <$> handOf p `shouldReturn` 0

    it "draws nothing when its cost is reduced to 0, as nothing was paid (FAQ)" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [force] <- asHulk p ["10006"]
        heli <- putIntoPlay p "01092"
        choose "use Helicarrier" (isAbilityOfCard heli)
        runNow [SupportMessage (supportOf heli) ReadiedSupport]
        choose "use Helicarrier again" (isAbilityOfCard heli)
        choose "attack" (isHeroAttack p)
        vid <- villainIdNow
        choose "Rhino" (isTargetChoice vid)
        choose "play Unstoppable Force" (isPlay force)
        isExhaustedNow p `shouldReturn` False
        handOf p `shouldReturn` []

  describe "Limitless Strength (10007)" do
    it "pays 3 [physical] in hero form" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [smash, limitless] <- asHulk p ["10003", "10007"]
        choose "attack" (isHeroAttack p)
        choose "Hulk Smash" (isPlay smash)
        choose "pay" (isPayWith limitless)
        pass
        vid <- villainIdNow
        choose "Rhino" (isTargetChoice vid)
        villainDamageNow `shouldReturn` 13

    it "cannot be spent by Bruce Banner" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        clearHand p
        lab <- putInHand p "10008"
        limitless <- putInHand p "10007"
        offers (isPlay lab) `shouldReturn` False
        _ <- putInHand p "01089"
        choose "play the lab" (isPlay lab)
        offers (isPayWith limitless) `shouldReturn` False

  describe "Banner's Laboratory (10008)" do
    it "gives Bruce Banner +2 REC and generates a [mental] resource in alter-ego form" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        clearHand p
        lab <- putIntoPlay p "10008"
        setDamage p 10
        choose "recover" isRecover
        damageOf p `shouldReturn` 4
        mansion <- putInHand p "01091"
        -- Three resources from cards require the Lab's mental resource to reach four.
        energies <- traverse (putInHand p) ["01088", "01086"]
        choose "play Avengers Mansion" (isPlay mansion)
        choose "pay with the lab" (isAbilityOfCard lab)
        for_ energies \c -> choose "pay" (isPayWith c)
        chooseIf isFinishPayment
        length <$> supportsWith "01091" `shouldReturn` 1

    it "generates nothing in hero form" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        lab <- putIntoPlay p "10008"
        _ <- asHulk p ["01088"]
        rage <- putInHand p "10009"
        choose "play Boundless Rage" (isPlay rage)
        offers (isAbilityOfCard lab) `shouldReturn` False

  describe "Boundless Rage (10009)" do
    it "gives Hulk +1 ATK, and is discarded when he changes form" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [rage, strength] <- asHulk p ["10009", "01090"]
        playWith rage [strength]
        upgradeCodes `shouldReturn` ["10009"]
        choose "attack" (isHeroAttack p)
        vid <- villainIdNow
        choose "Rhino" (isTargetChoice vid)
        villainDamageNow `shouldReturn` 4
        runNow [IdentityMessage p (ChangedToForm B)]
        choose "discard Boundless Rage" isAnyAbility
        upgradeCodes `shouldReturn` []

    it "is played only in hero form" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        clearHand p
        rage <- putInHand p "10009"
        _ <- putInHand p "01090"
        offers (isPlay rage) `shouldReturn` False

  describe "Immovable Object (10010)" do
    it "gives +4 hit points and Hulk retaliate 1" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        _ <- asHulk p []
        _ <- putIntoPlay p "10010"
        hitPointsOf p `shouldReturn` 22
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        choose "no defender" isNoDefender
        pass
        villainDamageNow `shouldReturn` 1

    it "gives Bruce Banner no retaliate" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "10010"
        hitPointsOf p `shouldReturn` 22
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        choose "no defender" isNoDefender
        pass
        villainDamageNow `shouldReturn` 0

  describe "Inner Demons (10025)" do
    it "turns Bruce Banner into Hulk, who is exhausted" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Obligations.innerDemons
        chooseIf isFormLabel
        pass
        a <- playerAttrs p
        a.playerIdentitySide `shouldBe` A
        isExhaustedNow p `shouldReturn` True
        getsGame (HashMap.null . gameObligations) `shouldReturn` True

    it "turns Hulk into Bruce Banner, who discards 2 cards" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [a1, a2, a3] <- asHulk p ["01088", "01089", "01090"]
        _ <- revealNow p Obligations.innerDemons
        chooseIf isFormLabel
        pass
        choose "discard" (isTargetChoice a1)
        choose "discard" (isTargetChoice a2)
        a <- playerAttrs p
        a.playerIdentitySide `shouldBe` B
        isExhaustedNow p `shouldReturn` False
        handOf p `shouldReturn` [a3]
 where
  isPayWith' = \case
    PayWithCard _ -> True
    _ -> False
