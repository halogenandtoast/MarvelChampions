module Marvel.Aspect.WaspPackSpec (spec) where

import Marvel.Ally.Types qualified as Ally
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as SideSchemes
import TestImport

wasp :: IO TestGame
wasp = testGame ["wasp"]

-- | Play a card from hand paying with new copies of the cards given.
playPaying :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
playPaying p card codes = do
  payment <- traverse (putInHand p) codes
  playWith card payment

-- | Wasp in her Tiny hero form, her hand empty and her turn's form change spent.
asHero :: IdentityId -> GameAppT ()
asHero p = do
  changeFormTo p A
  pass
  clearHand p

allyAttackNow, allyThwartNow, allyHitPointsNow :: CardCode -> GameAppT Natural
allyAttackNow code = Ally.getModifiedAttack =<< allyIn code
allyThwartNow code = Ally.getModifiedThwart =<< allyIn code
allyHitPointsNow code = Ally.getModifiedHitPoints =<< allyIn code

-- | The response of the card's own (its ability 1), not an ally's basic THW or ATK.
isResponseOf :: PlayerCard -> Choice -> Bool
isResponseOf c ch = isAbilityOfCard c ch && case ch of
  UseAbility ab -> abilityIndex ab == 1
  _ -> False

-- | Wasp back in alter-ego form on her next turn, after a turn in hero form: her hand
-- holds only the cards given, then she changes to Nadia Van Dyne.
toAlterEgoWith :: IdentityId -> [CardCode] -> GameAppT [PlayerCard]
toAlterEgoWith p codes = do
  asHero p
  endTurn
  pass
  clearHand p
  cards <- traverse (putInHand p) codes
  changeFormTo p B
  pure cards

isTough :: IdentityId -> GameAppT Bool
isTough p = (.playerIdentityTough) <$> playerAttrs p

spec :: Spec
spec = describe "The Wasp pack's aspect and basic cards" do
  describe "Thor, Jane Foster (13011)" do
    it "deals 2 damage to the villain after she is played from hand" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        clearHand p
        thor <- putInHand p "13011"
        playPaying p thor ["01088", "01088", "01089", "01089"]
        choose "Thor" (isAbilityOfCard thor)
        chooseVillain
        villainDamageNow `shouldReturn` 2

    it "deals 3 instead when paid for with a physical resource" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        clearHand p
        thor <- putInHand p "13011"
        playPaying p thor ["01090", "01088", "01089", "01089"]
        choose "Thor" (isAbilityOfCard thor)
        chooseVillain
        villainDamageNow `shouldReturn` 3

    it "put into play, not played, deals nothing" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        thor <- putIntoPlay p "13011"
        offers (isResponseOf thor) `shouldReturn` False
        villainDamageNow `shouldReturn` 0

  describe "Wasp, Janet Van Dyne (13012)" do
    it "gets a pym counter, and a hit point, for each energy resource overpaid" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        clearHand p
        jan <- putInHand p "13012"
        -- a basic Energy generates 2 energy
        playPaying p jan ["01088", "01089"]
        pass
        (.allyCounters) <$> allyIn "13012" `shouldReturn` 2
        allyHitPointsNow "13012" `shouldReturn` 2

    it "counts a wild resource declared as energy" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        clearHand p
        jan <- putInHand p "13012"
        payment <- traverse (putInHand p) ["13024", "01089"]
        playDeclaring jan payment Energy
        pass
        (.allyCounters) <$> allyIn "13012" `shouldReturn` 1
        allyHitPointsNow "13012" `shouldReturn` 1

    it "gets at most 3 counters" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        clearHand p
        jan <- putInHand p "13012"
        playPaying p jan ["01088", "01088"]
        pass
        (.allyCounters) <$> allyIn "13012" `shouldReturn` 3
        allyHitPointsNow "13012" `shouldReturn` 3

    it "with no energy overpaid, she has no hit points and is defeated" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        clearHand p
        jan <- putInHand p "13012"
        playPaying p jan ["01089", "01090"]
        pass
        alliesWith "13012" `shouldReturn` []

    it "put into play, nothing is paid: she has no hit points and is defeated" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "13012"
        alliesWith "13012" `shouldReturn` []

  describe "Into the Fray (13013)" do
    it "removes a threat from the main scheme for each point of excess damage" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 5
        before <- mainSchemeThreatNow
        mercenary <- addMinion p Minions.hydraMercenary
        fray <- putInHand p "13013"
        playPaying p fray ["01088", "01088", "01088"]
        choose "the mercenary" (isTargetChoice mercenary)
        pass
        minionDamageOf mercenary `shouldReturn` Nothing
        -- 6 damage to 3 hit points
        mainSchemeThreatNow `shouldReturn` before - 3

    it "removes none when a tough status card takes the damage" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 5
        before <- mainSchemeThreatNow
        sandman <- addMinion p Minions.sandman
        fray <- putInHand p "13013"
        playPaying p fray ["01088", "01088", "01088"]
        choose "Sandman" (isTargetChoice sandman)
        pass
        minionDamageOf sandman `shouldReturn` Just 0
        mainSchemeThreatNow `shouldReturn` before

    it "removes no threat while a crisis icon is in play" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 5
        _ <- revealNow p SideSchemes.crowdControl
        pass
        before <- mainSchemeThreatNow
        mercenary <- addMinion p Minions.hydraMercenary
        fray <- putInHand p "13013"
        playPaying p fray ["01088", "01088"]
        choose "the mercenary" (isTargetChoice mercenary)
        pass
        minionDamageOf mercenary `shouldReturn` Nothing
        mainSchemeThreatNow `shouldReturn` before

    it "removes the excess once, though the defeat window opens again after a forced interrupt" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 5
        before <- mainSchemeThreatNow
        beetle <- addMinion p Minions.beetle
        fray <- putInHand p "13013"
        playPaying p fray ["01088", "01088"]
        choose "Beetle" (isTargetChoice beetle)
        -- Beetle's Forced Interrupt answers the window (no physical: he is shuffled in),
        -- and the window opens again
        toFirstTurn
        -- 6 damage to 4 hit points
        mainSchemeThreatNow `shouldReturn` before - 2

  describe "Surprise Attack (13014)" do
    it "after you change form, deals 3 damage to an enemy" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        clearHand p
        surprise <- putInHand p "13014"
        mental <- putInHand p "01089"
        offers (isPlay surprise) `shouldReturn` False
        changeFormTo p A
        choose "Surprise Attack" (isPlay surprise)
        payWith [mental]
        chooseVillain
        villainDamageNow `shouldReturn` 3

    it "deals 4 instead when paid for with a physical resource" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        clearHand p
        surprise <- putInHand p "13014"
        physical <- putInHand p "01090"
        changeFormTo p A
        choose "Surprise Attack" (isPlay surprise)
        payWith [physical]
        chooseVillain
        villainDamageNow `shouldReturn` 4

    it "is not played after changing to alter-ego form" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        [surprise, _] <- toAlterEgoWith p ["13014", "01090"]
        offers (isPlay surprise) `shouldReturn` False

  describe "Boot Camp (13016)" do
    it "gives each ally its controller controls +1 ATK, not another player's" do
      tg <- testGame ["wasp", "captain-marvel"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        _ <- putIntoPlay p "13018"
        _ <- putIntoPlay other "01068"
        allyAttackNow "13018" `shouldReturn` 1
        _ <- putIntoPlay p "13016"
        allyAttackNow "13018" `shouldReturn` 2
        allyAttackNow "01068" `shouldReturn` 2

    it "played under another player's control, gives that player's allies +1 ATK" do
      tg <- testGame ["wasp", "captain-marvel"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "13018"
        _ <- putIntoPlay other "01068"
        camp <- putInHand p "13016"
        payment <- traverse (putInHand p) ["01089", "01089"]
        playOn camp other payment
        [c] <- supportsWith "13016"
        c.supportController `shouldBe` other
        allyAttackNow "01068" `shouldReturn` 3
        allyAttackNow "13018" `shouldReturn` 1

    it "is limited to 1 per player" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "13016"
        second <- putInHand p "13016"
        _ <- putInHand p "01089"
        _ <- putInHand p "01089"
        offers (isPlay second) `shouldReturn` False

  describe "Lie in Wait (13017)" do
    it "after a minion engages you, is discarded to deal 3 damage to that minion" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        asHero p
        lie <- putIntoPlay p "13017"
        loki <- minionOf <$> revealNow p Minions.loki
        choose "Lie in Wait" (isAbilityOfCard lie)
        pass
        minionDamageOf loki `shouldReturn` Just 3
        null <$> upgradesWith "13017" `shouldReturn` True
        map toCardCode <$> discardOf p >>= (`shouldSatisfy` elem "13017")

    it "is a Hero Response: not used in alter-ego form" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        lie <- putIntoPlay p "13017"
        _ <- revealNow p Minions.loki
        offers (isAbilityOfCard lie) `shouldReturn` False

    it "is limited to 1 per player" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "13017"
        second <- putInHand p "13017"
        _ <- putInHand p "01088"
        offers (isPlay second) `shouldReturn` False

  describe "Ironheart (13018)" do
    it "draws 1 card after she is played from hand" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- stackDeck p ["01088"]
        ironheart <- putInHand p "13018"
        playPaying p ironheart ["01089", "01089"]
        choose "Ironheart" (isAbilityOfCard ironheart)
        map toCardCode <$> handOf p `shouldReturn` ["01088"]

    it "put into play, draws nothing" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        clearHand p
        ironheart <- putIntoPlay p "13018"
        offers (isResponseOf ironheart) `shouldReturn` False
        handOf p `shouldReturn` []

  describe "Spider-Man, Miles Morales (13019)" do
    it "gets +2 ATK until the end of the phase, if chosen" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        clearHand p
        miles <- putInHand p "13019"
        playPaying p miles ["01089", "01089", "01089"]
        choose "Spider-Man" (isAbilityOfCard miles)
        choose "ATK" (isLabel "$label.attack")
        allyAttackNow "13019" `shouldReturn` 4
        allyThwartNow "13019" `shouldReturn` 1
        endTurn
        allyAttackNow "13019" `shouldReturn` 2

    it "gets +2 THW, if chosen" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        clearHand p
        miles <- putInHand p "13019"
        playPaying p miles ["01089", "01089", "01089"]
        choose "Spider-Man" (isAbilityOfCard miles)
        choose "THW" (isLabel "$label.thwart")
        allyThwartNow "13019" `shouldReturn` 3
        allyAttackNow "13019" `shouldReturn` 2

    it "put into play, gets nothing" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        miles <- putIntoPlay p "13019"
        offers (isResponseOf miles) `shouldReturn` False
        allyAttackNow "13019" `shouldReturn` 2
        allyThwartNow "13019" `shouldReturn` 1

  describe "The Power in All of Us (13024)" do
    it "generates one resource for a card that is not basic" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        clearHand p
        -- Thor costs 4: 1 from it and 2 from a basic Energy
        thor <- putInHand p "13011"
        _ <- putInHand p "13024"
        _ <- putInHand p "01088"
        offers (isPlay thor) `shouldReturn` False

    it "pays for a basic card costing 2 on its own" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        clearHand p
        ironheart <- putInHand p "13018"
        playPaying p ironheart ["13024"]
        chooseIf (isAbilityOfCard ironheart)
        length <$> alliesWith "13018" `shouldReturn` 1

  describe "Running Interference (13031)" do
    it "removes 2 threat from the main scheme, plus the villain's stage number" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 6
        before <- mainSchemeThreatNow
        running <- putInHand p "13031"
        playPaying p running ["01088", "01088"]
        pass
        -- Rhino I: 2 + 1
        mainSchemeThreatNow `shouldReturn` before - 3

    it "removes no threat while a crisis icon is in play" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 6
        _ <- revealNow p SideSchemes.crowdControl
        pass
        before <- mainSchemeThreatNow
        running <- putInHand p "13031"
        playPaying p running ["01088"]
        pass
        mainSchemeThreatNow `shouldReturn` before

    it "cannot be played without the Avenger trait" do
      tg <- testGame ["ms-marvel"]
      let p = player1 tg
      inGame tg do
        changeForm p
        pass
        clearHand p
        placeMainThreat 6
        running <- putInHand p "13031"
        _ <- putInHand p "01088"
        _ <- putInHand p "01088"
        offers (isPlay running) `shouldReturn` False

  describe "All for One (13032)" do
    it "deals 1 more damage for each Avenger character exhausted, the hero among them" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        asHero p
        thor <- putIntoPlay p "13011"
        ironheart <- putIntoPlay p "13018"
        allForOne <- putInHand p "13032"
        playPaying p allForOne ["01088", "01088"]
        chooseVillain
        -- Ironheart is no Avenger
        offers (isTargetChoice (allyIdOf ironheart)) `shouldReturn` False
        choose "exhaust Thor" (isTargetChoice (allyIdOf thor))
        choose "exhaust Wasp" (isTargetChoice p)
        pass
        villainDamageNow `shouldReturn` 5
        exhaustedNow p `shouldReturn` True
        (.allyExhausted) <$> allyIn "13011" `shouldReturn` True
        (.allyExhausted) <$> allyIn "13018" `shouldReturn` False

    it "exhausting none deals 3" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "13011"
        allForOne <- putInHand p "13032"
        playPaying p allForOne ["01088", "01088"]
        chooseVillain
        choose "done" (isLabel "$label.done")
        villainDamageNow `shouldReturn` 3
        exhaustedNow p `shouldReturn` False
        (.allyExhausted) <$> allyIn "13011" `shouldReturn` False

  describe "Perseverance (13033)" do
    it "after you change form, gives your hero a tough status card" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        clearHand p
        perseverance <- putInHand p "13033"
        mental <- putInHand p "01089"
        offers (isPlay perseverance) `shouldReturn` False
        changeFormTo p A
        choose "Perseverance" (isPlay perseverance)
        payWith [mental]
        isTough p `shouldReturn` True

    it "is not played after changing to alter-ego form" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        [perseverance, _] <- toAlterEgoWith p ["13033", "01089"]
        offers (isPlay perseverance) `shouldReturn` False
        isTough p `shouldReturn` False

  describe "Athletic Conditioning (13034)" do
    it "discards a stun status card from your hero" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        asHero p
        resolve [IdentityMessage p IdentityStunned]
        conditioning <- putInHand p "13034"
        playPaying p conditioning ["01089"]
        isStunned p `shouldReturn` False

    it "with both, discards the one chosen" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        asHero p
        resolve [IdentityMessage p IdentityStunned, IdentityMessage p IdentityConfused]
        conditioning <- putInHand p "13034"
        playPaying p conditioning ["01089"]
        choose "confused" (isLabel "$label.confusedStatus")
        isConfused p `shouldReturn` False
        isStunned p `shouldReturn` True
