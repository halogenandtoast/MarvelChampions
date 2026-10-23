module Marvel.Hero.MsMarvelSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

msMarvel :: IO TestGame
msMarvel = testGame ["ms-marvel"]

-- | Play a card from hand paying with new copies of the cards given.
playPaying :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
playPaying p card codes = do
  payment <- traverse (putInHand p) codes
  playWith card payment

-- | A card's ability by its index on the card.
isAbility :: PlayerCard -> Natural -> Choice -> Bool
isAbility c n = \case
  choice@(UseAbility ab) -> isAbilityOfCard c choice && abilityIndex ab == n
  _ -> False

codes :: [PlayerCard] -> [CardCode]
codes = map toCardCode

spec :: Spec
spec = describe "Ms. Marvel" do
  describe "Ms. Marvel (05001a)" do
    it "Morphogenetics: after she plays an Attack event, exhaust her to return it to hand" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        hands <- putInHand p "05003"
        playPaying p hands ["01088"]
        vid <- villainIdNow
        choose "hit Rhino" (isTargetChoice vid)
        villainDamageNow `shouldReturn` 4
        choose "morphogenetics" (isNamedAbility "Morphogenetics")
        exhaustedNow p `shouldReturn` True
        map pcCardId <$> handOf p `shouldReturn` [pcCardId hands]
        codes <$> discardOf p >>= (`shouldSatisfy` notElem "05003")

    it "Morphogenetics answers Thwart and Defense events, not others" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        placeMainThreat 5
        sneak <- putInHand p "05004"
        playPaying p sneak ["01089"]
        chooseMainScheme
        offers (isNamedAbility "Morphogenetics") `shouldReturn` True
        pass
        boost <- putInHand p "05032"
        playPaying p boost ["01088"]
        choose "the target" (isTargetChoice p)
        offers (isNamedAbility "Morphogenetics") `shouldReturn` False

    -- the Ms. Marvel pack's printing is the core set's card, with no trait (MarvelCDB's
    -- data gives it Thwart; the printed card and FAQ do not)
    it "Morphogenetics does not answer Get Behind Me! (05013), which has no trait" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        behind <- putInHand p "05013"
        payment <- putInHand p "01089"
        _ <- revealNow p Treacheries.advance
        playWith behind [payment]
        choose "take the attack" isNoDefender
        offers (isNamedAbility "Morphogenetics") `shouldReturn` False

    it "Morphogenetics costs exhausting her" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        resolve [IdentityMessage p ExhaustedIdentity]
        hands <- putInHand p "05003"
        playPaying p hands ["01088"]
        vid <- villainIdNow
        choose "hit Rhino" (isTargetChoice vid)
        offers (isNamedAbility "Morphogenetics") `shouldReturn` False

  describe "Kamala Khan (05001b)" do
    it "Teen Spirit: discards from the deck until a Ms. Marvel card, which goes to hand, once a round" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- stackDeck p ["01088", "05019", "05006", "05003"]
        choose "teen spirit" (isNamedAbility "Teen Spirit")
        codes <$> handOf p `shouldReturn` ["05006"]
        take 2 . codes <$> discardOf p `shouldReturn` ["05019", "01088"]
        offers (isNamedAbility "Teen Spirit") `shouldReturn` False

    it "Teen Spirit is not used as Ms. Marvel" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        offers (isNamedAbility "Teen Spirit") `shouldReturn` False

  describe "Red Dagger (05002)" do
    it "defeated, spends 2 resources of different types to deal 2 damage and return to hand" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        clearHand p
        dagger <- putIntoPlay p "05002"
        energy <- putInHand p "05019"
        genius <- putInHand p "05020"
        start [AllyMessage (allyIdOf dagger) (AllyDamaged (toSource p) (toDamage 3 FromAbility))]
        choose "use Red Dagger" (isAbilityOfCard dagger)
        choose "pay energy" (isPayWith energy)
        -- two energy are one type
        offers isFinishPayment `shouldReturn` False
        choose "pay genius" (isPayWith genius)
        chooseIf isFinishPayment
        chooseVillain
        villainDamageNow `shouldReturn` 2
        alliesWith "05002" `shouldReturn` []
        map pcCardId <$> handOf p `shouldReturn` [pcCardId dagger]
        codes <$> discardOf p >>= (`shouldSatisfy` notElem "05002")

    it "is not offered without resources of two types" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        clearHand p
        dagger <- putIntoPlay p "05002"
        _ <- putInHand p "05019"
        start [AllyMessage (allyIdOf dagger) (AllyDamaged (toSource p) (toDamage 3 FromAbility))]
        offers (isAbilityOfCard dagger) `shouldReturn` False
        pass
        alliesWith "05002" `shouldReturn` []
        filter (== "05002") . codes <$> discardOf p `shouldReturn` ["05002"]

  describe "Big Hands (05003)" do
    it "deals 4 damage to an enemy" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        hands <- putInHand p "05003"
        offers (isPlay hands) `shouldReturn` False
        playPaying p hands ["01088"]
        vid <- villainIdNow
        choose "hit Rhino" (isTargetChoice vid)
        villainDamageNow `shouldReturn` 4

  describe "Sneak By (05004)" do
    it "removes 3 threat from a scheme" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        placeMainThreat 5
        before <- mainSchemeThreat
        sneak <- putInHand p "05004"
        playPaying p sneak ["01089"]
        chooseMainScheme
        mainSchemeThreat `shouldReturn` (before - 3)

  describe "Wiggle Room (05005)" do
    it "prevents 3 of the damage you would take, and draws a card" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        wiggle <- putInHand p "05005"
        vid <- villainIdNow
        start (damageIdentity p (toSource vid) (toDamage 5 FromAbility))
        choose "play Wiggle Room" (isPlay wiggle)
        pass
        damageOf p `shouldReturn` 2
        length <$> handOf p `shouldReturn` 1

    it "is a hero interrupt" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        clearHand p
        wiggle <- putInHand p "05005"
        vid <- villainIdNow
        start (damageIdentity p (toSource vid) (toDamage 5 FromAbility))
        offers (isPlay wiggle) `shouldReturn` False

  describe "Aamir Khan (05006)" do
    it "puts a card from the discard pile on the bottom of the deck, then draws one" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        clearHand p
        aamir <- putIntoPlay p "05006"
        [card] <- stackDiscard p ["01088"]
        choose "use Aamir" (isAbilityOfCard aamir)
        choose "the card" (isTargetChoice card)
        lastMay . unDeck . (.playerIdentityDeck) <$> playerAttrs p `shouldReturn` Just card
        discardOf p `shouldReturn` []
        length <$> handOf p `shouldReturn` 1

    it "is an alter-ego action" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        aamir <- putIntoPlay p "05006"
        changeForm p
        offers (isAbilityOfCard aamir) `shouldReturn` False

  describe "Bruno Carrelli (05007)" do
    it "holds a card from hand facedown, and gives back up to 3" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        clearHand p
        bruno <- putIntoPlay p "05007"
        energy <- putInHand p "01088"
        choose "hide a card" (isAbility bruno 1)
        choose "the card" (isTargetChoice energy)
        handOf p `shouldReturn` []
        map (.supportHeldCards) <$> supportsWith "05007" `shouldReturn` [[energy]]
        resolve [SupportMessage (supportIdOf bruno) ReadiedSupport]
        changeForm p
        choose "take them back" (isAbility bruno 2)
        choose "the card" (isTargetChoice energy)
        handOf p `shouldReturn` [energy]
        map (.supportHeldCards) <$> supportsWith "05007" `shouldReturn` [[]]

    it "hides a card in alter-ego form only" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        bruno <- putIntoPlay p "05007"
        changeForm p
        offers (isAbility bruno 1) `shouldReturn` False

    it "discards the cards it holds as it leaves play" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        clearHand p
        bruno <- putIntoPlay p "05007"
        energy <- putInHand p "01088"
        choose "hide a card" (isAbility bruno 1)
        choose "the card" (isTargetChoice energy)
        resolve [RemoveFromPlay (SupportRef $ supportIdOf bruno)]
        discardOf p >>= (`shouldSatisfy` elem energy)

  describe "Nakia Bahadir (05008)" do
    it "reduces the cost of the next card you play this phase by 1" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        clearHand p
        nakia <- putIntoPlay p "05008"
        aamir <- putInHand p "05006"
        offers (isPlay aamir) `shouldReturn` False
        choose "use Nakia" (isAbilityOfCard nakia)
        playWith aamir []
        supportsWith "05006" >>= (`shouldSatisfy` ((== 1) . length))

  describe "Biokinetic Polymer Suit (05009)" do
    it "generates a wild resource for an event only" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        suit <- putIntoPlay p "05009"
        aamir <- putInHand p "05006"
        offers (isPlay aamir) `shouldReturn` False
        clearHand p
        boost <- putInHand p "05032"
        offers (isPlay boost) `shouldReturn` True
        choose "play Morale Boost" (isPlay boost)
        choose "pay with the suit" (isAbilityOfCard suit)
        chooseIf isFinishPayment
        choose "the target" (isTargetChoice p)
        upgradeIsExhausted (upgradeIdOf suit) `shouldReturn` True
        codes <$> discardOf p >>= (`shouldSatisfy` elem "05032")

  describe "Embiggen! (05010)" do
    it "increases the damage an Attack event deals by 2" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        embiggen <- putIntoPlay p "05010"
        hands <- putInHand p "05003"
        playPaying p hands ["01088"]
        choose "embiggen" (isAbilityOfCard embiggen)
        vid <- villainIdNow
        choose "hit Rhino" (isTargetChoice vid)
        villainDamageNow `shouldReturn` 6
        upgradeIsExhausted (upgradeIdOf embiggen) `shouldReturn` True

    it "increases each instance of damage the event deals" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        mercenary <- addMinion p Minions.hydraMercenary
        pass
        embiggen <- putIntoPlay p "05010"
        melee <- putInHand p "05030"
        playPaying p melee ["01089", "01090"]
        choose "embiggen" (isAbilityOfCard embiggen)
        -- the mercenary guards Rhino until the first damage defeats it
        vid <- villainIdNow
        offers (isTargetChoice (EnemyVillainId vid)) `shouldReturn` False
        choose "hit the mercenary" (isTargetChoice (EnemyMinionId mercenary))
        choose "hit Rhino" (isTargetChoice vid)
        villainDamageNow `shouldReturn` 5
        minionDamageOf mercenary `shouldReturn` Nothing

    it "does not answer an event without the Attack trait, and lasts only for its event" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        embiggen <- putIntoPlay p "05010"
        boost <- putInHand p "05032"
        playPaying p boost ["01088"]
        offers (isAbilityOfCard embiggen) `shouldReturn` False

  describe "Shrink (05011)" do
    it "increases the threat a Thwart event removes by 2" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        placeMainThreat 5
        before <- mainSchemeThreat
        shrink <- putIntoPlay p "05011"
        sneak <- putInHand p "05004"
        playPaying p sneak ["01089"]
        choose "shrink" (isAbilityOfCard shrink)
        chooseMainScheme
        mainSchemeThreat `shouldReturn` (before - 5)

  describe "Home by Dawn (05025)" do
    it "exhausts Kamala Khan to be removed from the game" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        dawn <- revealNow p Obligations.homeByDawn
        choose "exhaust Kamala" (isLabel "$label.exhaustKamalaKhanRemoveHomeByDawn")
        isExhaustedNow p `shouldReturn` True
        getsGame (HashMap.null . gameObligations) `shouldReturn` True
        (map ecCardId <$> encounterDiscard) >>= (`shouldSatisfy` notElem (ecCardId dawn))

    it "lets Ms. Marvel flip to Kamala Khan first" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        _ <- revealNow p Obligations.homeByDawn
        choose "flip" (isLabel "$label.changeToB")
        choose "exhaust Kamala" (isLabel "$label.exhaustKamalaKhanRemoveHomeByDawn")
        (.playerIdentitySide) <$> playerAttrs p `shouldReturn` B

    it "otherwise discards a Persona support, and is discarded" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        aamir <- putIntoPlay p "05006"
        dawn <- revealNow p Obligations.homeByDawn
        choose "stay" (isLabel "$label.doNotFlipToAlterEgo")
        offers (isLabel "$label.exhaustKamalaKhanRemoveHomeByDawn") `shouldReturn` False
        chooseIf (isLabel "$label.discardPersonaSupportDiscardObligation")
        choose "the target" (isTargetChoice (supportIdOf aamir))
        pass
        supportsWith "05006" `shouldReturn` []
        filter (== "05006") . codes <$> discardOf p `shouldReturn` ["05006"]
        (map ecCardId <$> encounterDiscard) >>= (`shouldSatisfy` elem (ecCardId dawn))

    it "surges when no Persona support is discarded" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        [next] <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Obligations.homeByDawn
        choose "stay" (isLabel "$label.doNotFlipToAlterEgo")
        pass
        dealtTo p >>= (`shouldSatisfy` (ecCardId next `elem`) . map ecCardId)
