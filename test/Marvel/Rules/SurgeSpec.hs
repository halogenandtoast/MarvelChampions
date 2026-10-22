module Marvel.Rules.SurgeSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

inPlay :: EncounterCard -> GameAppT Bool
inPlay card = getsGame (HashMap.member (minionOf card) . gameMinions)

-- | Pay for the card in hand with the card given.
playPaying :: PlayerCard -> PlayerCard -> GameAppT ()
playPaying card payment = do
  choose "play" (isPlay card)
  choose "pay" (isPayWith payment)
  chooseIf isFinishPayment

spec :: Spec
spec = describe "surge" do
  it "deals a facedown encounter card, revealed in the villain phase" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      [next] <- stackEncounterDeck [Minions.hydraMercenary]
      before <- mainSchemeThreat
      _ <- revealNow p Treacheries.kreeManipulator
      pass
      mainSchemeThreat `shouldReturn` before + 1
      dealtTo p `shouldReturn` [next]
      inPlay next `shouldReturn` False

  it "a card it deals as dealt cards are revealed is revealed after them" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      [kree, mercenary] <- traverse encounterCard [Treacheries.kreeManipulator, Minions.hydraMercenary]
      ([next], _) <- setEncounterDeck [Minions.hydraMercenary] [Treacheries.advance]
      resolve [IdentityMessage p (DealtEncounterCard kree), IdentityMessage p (DealtEncounterCard mercenary), RevealDealtEncounterCards]
      dealtTo p `shouldReturn` []
      inPlay mercenary `shouldReturn` True
      inPlay next `shouldReturn` True

  describe "cancelling a treachery's When Revealed effects cancels its surge" do
    it "Enhanced Spider-Sense (01004): the surge keyword" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        sense <- putInHand p "01004"
        payment <- putInHand p "01088"
        before <- mainSchemeThreat
        _ <- revealNow p Treacheries.kreeManipulator
        playPaying sense payment
        pass
        mainSchemeThreat `shouldReturn` before
        dealtTo p `shouldReturn` []

    it "Enhanced Spider-Sense (01004): the surge it would gain" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        sense <- putInHand p "01004"
        payment <- putInHand p "01088"
        _ <- revealNow p Treacheries.clashOfTheTitans
        playPaying sense payment
        pass
        damageOf p `shouldReturn` 0
        dealtTo p `shouldReturn` []

    it "Get Behind Me! (01078): the villain attacks instead, and nothing surges" do
      tg <- testGame ["black-panther"]
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        behind <- putInHand p "01078"
        payment <- putInHand p "01089"
        before <- mainSchemeThreat
        _ <- revealNow p Treacheries.kreeManipulator
        playPaying behind payment
        choose "take the attack" isNoDefender
        pass
        mainSchemeThreat `shouldReturn` before
        damageOf p >>= (`shouldSatisfy` (> 0))
        dealtTo p `shouldReturn` []

    it "Protective Ward (09007): all of its effects" do
      tg <- testGame ["doctor-strange"]
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        ward <- putInHand p "09007"
        payment <- putInHand p "01090"
        before <- mainSchemeThreat
        kree <- revealNow p Treacheries.kreeManipulator
        playPaying ward payment
        pass
        mainSchemeThreat `shouldReturn` before
        dealtTo p `shouldReturn` []
        encounterDiscardNow >>= (`shouldSatisfy` elem kree)

  describe "cancelling a card's effects cancels its surge" do
    it "Black Widow (01075)" do
      tg <- testGame ["black-panther"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "01075"
        widow <- allyIn "01075"
        genius <- putInHand p "01089"
        [next] <- stackEncounterDeck [Treacheries.advance]
        kree <- encounterCard Treacheries.kreeManipulator
        before <- mainSchemeThreat
        runNow [RevealEncounterCard p kree]
        choose "use Black Widow" (isAbilityOf (allyId widow))
        choose "pay" (isPayWith genius)
        chooseIf isFinishPayment
        resolve []
        -- Advance is revealed in its place, and nothing is dealt
        mainSchemeThreat >>= (`shouldSatisfy` (> before))
        encounterDiscardNow >>= (`shouldSatisfy` \d -> kree `elem` d && next `elem` d)
        dealtTo p `shouldReturn` []

    it "Spycraft (08018)" do
      tg <- testGame ["black-widow"]
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        spycraft <- putIntoPlay p "08018"
        [next] <- stackEncounterDeck [Minions.hydraMercenary]
        kree <- encounterCard Treacheries.kreeManipulator
        before <- mainSchemeThreat
        runNow [RevealEncounterCard p kree]
        choose "use Spycraft" (isAbilityOfCard spycraft)
        pass
        mainSchemeThreat `shouldReturn` before
        inPlay next `shouldReturn` True
        dealtTo p `shouldReturn` []

    it "Grappling Hook (08008)" do
      tg <- testGame ["black-widow"]
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        hook <- putIntoPlay p "08008"
        kree <- encounterCard Treacheries.kreeManipulator
        before <- mainSchemeThreat
        runNow [RevealEncounterCard p kree]
        choose "use Grappling Hook" (isAbilityOfCard hook)
        pass
        mainSchemeThreat `shouldReturn` before
        dealtTo p `shouldReturn` []

  describe "Espionage (08033)" do
    it "answers the surge keyword" do
      tg <- testGame ["black-widow"]
      let p = player1 tg
      inGame tg do
        clearHand p
        espionage <- putIntoPlay p "08033"
        _ <- revealNow p Treacheries.kreeManipulator
        choose "use Espionage" (isAbilityOfCard espionage)
        pass
        length <$> handOf p `shouldReturn` 2

    it "does not answer an obligation revealed with no player to give it to" do
      tg <- testGame ["black-widow"]
      let p = player1 tg
      inGame tg do
        clearHand p
        espionage <- putIntoPlay p "08033"
        [next] <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Obligations.evictionNotice
        offers (isAbilityOfCard espionage) `shouldReturn` False
        pass
        -- another card is revealed at once instead
        inPlay next `shouldReturn` True
