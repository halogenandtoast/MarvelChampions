module Marvel.Campaign.MarketCheapEventsSpec (spec) where

import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as V
import TestImport

-- | Captain Marvel in hero form, her hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

villainField :: Field Villain a -> GameAppT a
villainField f = field f <$> getVillain

-- | Decline response windows until the choice is offered.
passUntil :: (HasCallStack) => (Choice -> Bool) -> GameAppT ()
passUntil f =
  offers f >>= \case
    True -> pure ()
    False -> do
      offers isNoResponse `shouldReturn` True
      choose "decline" isNoResponse
      passUntil f

neverOffered :: (HasCallStack) => (Choice -> Bool) -> GameAppT ()
neverOffered f = do
  offers f `shouldReturn` False
  whenM (offers isNoResponse) $ choose "decline" isNoResponse >> neverOffered f

-- | Rhino (2 ATK) attacks the player undefended, the boost card on top of the deck.
rhinoAttacks :: IdentityId -> CardDef -> GameAppT ()
rhinoAttacks p boost = do
  _ <- stackEncounterDeck [boost]
  vid <- villainIdNow
  start [VillainMessage vid (VillainAttacks p)]
  choose "no defender" isNoDefender

-- | Play a card from hand, paying with a new Energy card.
playPayingEnergy :: IdentityId -> PlayerCard -> GameAppT ()
playPayingEnergy p card = do
  energy <- putInHand p "01088"
  playWith card [energy]

handCount :: IdentityId -> GameAppT Int
handCount p = length <$> handOf p

spec :: Spec
spec = describe "The Market: cheap events" do
  describe "Brainstorm (16150)" do
    it "naming the top card's type thwarts 3 from the main scheme; the card goes to the bottom, then draw" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 6
        threat <- mainSchemeThreatNow
        [energy, next] <- stackDeck p ["01088", "01089"]
        card <- putInHand p "16150"
        playWith card []
        choose "name resource" (isLabel "$label.nameResourceType")
        pass
        choose "the bottom" (isLabel "$label.placeOnBottomOfDeck")
        pass
        mainSchemeThreatNow `shouldReturn` (threat - 3)
        handOf p `shouldReturn` [next]
        lastMay . unDeck . (.playerIdentityDeck) <$> playerAttrs p `shouldReturn` Just energy

    it "thwarts nothing while a patrol minion is engaged with you (RR Patrol)" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 6
        _ <- addMinion p Minions.headhuntersHenchman
        threat <- mainSchemeThreatNow
        _ <- stackDeck p ["01088", "01089"]
        card <- putInHand p "16150"
        playWith card []
        choose "name resource" (isLabel "$label.nameResourceType")
        pass
        choose "the bottom" (isLabel "$label.placeOnBottomOfDeck")
        mainSchemeThreatNow `shouldReturn` threat

    it "another type thwarts nothing; on top, it is the card drawn" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 6
        threat <- mainSchemeThreatNow
        [energy, _] <- stackDeck p ["01088", "01089"]
        card <- putInHand p "16150"
        playWith card []
        offers (isLabel "$label.nameMinionType") `shouldReturn` False
        choose "name ally" (isLabel "$label.nameAllyType")
        choose "the top" (isLabel "$label.placeOnTopOfDeck")
        pass
        mainSchemeThreatNow `shouldReturn` threat
        handOf p `shouldReturn` [energy]

  describe "By Any Means (16151)" do
    it "places 2 threat on the main scheme, deals 3 damage to the villain and draws 1" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        threat <- mainSchemeThreatNow
        card <- putInHand p "16151"
        playWith card []
        pass
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 3
        mainSchemeThreatNow `shouldReturn` (threat + 2)
        handCount p `shouldReturn` 1

  describe "Contingency Plan (16152)" do
    it "discards the top 4 cards: 1 damage for each different resource type among them" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        stacked <- stackDeck p ["01088", "01089", "01088", "01089"]
        card <- putInHand p "16152"
        playWith card []
        pass
        -- energy and mental: two instances of 1 damage
        chooseVillain
        pass
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 2
        discarded <- discardOf p
        stacked `shouldSatisfy` all (`elem` discarded)
        handCount p `shouldReturn` 1

  describe "In Defiance (16153)" do
    it "prevents 2 of the damage an attack would deal an identity, and draws 1" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        card <- putInHand p "16153"
        -- Rhino's 2 ATK and Stampede's 1 boost
        rhinoAttacks p Treacheries.stampede
        passUntil (isPlay card)
        choose "play In Defiance" (isPlay card)
        pass
        damageOf p `shouldReturn` 1
        handCount p `shouldReturn` 1

    it "is not offered for damage that is not from an attack" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 6
        card <- putInHand p "16153"
        harm <- putInHand p "16160"
        playPayingEnergy p harm
        neverOffered (isPlay card)
        damageOf p `shouldReturn` 2

  describe "Calculate the Odds (16154)" do
    it "draws 1; the player chosen draws 1, then chooses and discards 1" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        [energy, mental] <- stackDeck p ["01088", "01089"]
        card <- putInHand p "16154"
        playWith card []
        -- the only player: chosen without asking
        chooseIf (isTargetChoice p)
        choose "draw, then discard" (isLabel "$label.drawThenDiscard")
        choose "discard the energy" (isTargetChoice energy)
        pass
        handOf p `shouldReturn` [mental]
        (`shouldSatisfy` elem energy) =<< discardOf p

    it "the player chosen may decline" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        [energy, _] <- stackDeck p ["01088", "01089"]
        card <- putInHand p "16154"
        playWith card []
        -- the only player: chosen without asking
        chooseIf (isTargetChoice p)
        choose "decline" (isLabel "$label.pass")
        pass
        handOf p `shouldReturn` [energy]

    it "another player chosen draws and discards" do
      tg <- testGame ["captain-marvel", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        asHero p
        before <- handCount other
        card <- putInHand p "16154"
        playWith card []
        choose "choose the other player" (isTargetChoice other)
        fmap fst <$> asked `shouldReturn` Just other
        choose "draw, then discard" (isLabel "$label.drawThenDiscard")
        hand <- handOf other
        choose "discard" (isTargetChoice (fromMaybe (error "no cards") (headMay hand)))
        pass
        handCount other `shouldReturn` before
        handCount p `shouldReturn` 1

  describe "Creative Solution (16155)" do
    it "removing the villain's tough deals 3 damage to an enemy" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        vid <- villainIdNow
        resolve [GiveStatus (VillainRef vid) ToughStatus]
        card <- putInHand p "16155"
        playWith card []
        choose "the villain" (isTargetChoice vid)
        choose "its tough" (isLabel "$label.toughStatus")
        chooseVillain
        pass
        villainField V.VillainTough `shouldReturn` False
        villainDamageNow `shouldReturn` 3
        handCount p `shouldReturn` 1

    it "removing a stun removes 3 threat from a scheme" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 6
        threat <- mainSchemeThreatNow
        resolve [GiveStatus (IdentityRef p) StunnedStatus]
        card <- putInHand p "16155"
        playWith card []
        -- the only character with a status card: chosen without asking
        chooseIf (isTargetChoice p)
        choose "her stun" (isLabel "$label.stunnedStatus")
        chooseMainScheme
        pass
        isStunned p `shouldReturn` False
        mainSchemeThreatNow `shouldReturn` (threat - 3)

    it "removing a confuse heals 3 damage from an identity" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        setDamage p 4
        resolve [GiveStatus (IdentityRef p) ConfusedStatus]
        card <- putInHand p "16155"
        playWith card []
        -- the only character with a status card: chosen without asking
        chooseIf (isTargetChoice p)
        choose "her confuse" (isLabel "$label.confusedStatus")
        chooseIf (isTargetChoice p)
        pass
        isConfused p `shouldReturn` False
        damageOf p `shouldReturn` 1

    it "with no status card in play only draws" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        card <- putInHand p "16155"
        playWith card []
        pass
        handCount p `shouldReturn` 1
        villainDamageNow `shouldReturn` 0

  describe "Grapple (16156)" do
    it "deals 1 damage to an enemy and stuns it, stuns your hero, draws 1" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        card <- putInHand p "16156"
        playWith card []
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 1
        villainField V.VillainStunned `shouldReturn` True
        isStunned p `shouldReturn` True
        handCount p `shouldReturn` 1

  describe "Wing It (16157)" do
    it "deals 1 damage to an enemy and confuses it, confuses your hero, draws 1" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        card <- putInHand p "16157"
        playWith card []
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 1
        villainField V.VillainConfused `shouldReturn` True
        isConfused p `shouldReturn` True
        handCount p `shouldReturn` 1

  describe "Close Call (16158)" do
    it "cancels a boost card's icons as the villain attacks, discards it and draws 1" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        card <- putInHand p "16158"
        rhinoAttacks p Treacheries.stampede
        passUntil (isPlay card)
        choose "play Close Call" (isPlay card)
        pass
        -- Rhino's 2 ATK alone
        damageOf p `shouldReturn` 2
        encounterDiscardCodes >>= (`shouldSatisfy` elem (toCardCode Treacheries.stampede))
        handCount p `shouldReturn` 1

    it "cancels a boost card's \"Boost\" ability as the villain schemes" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        -- an upgrade for Taskmaster's boost to count
        _ <- putIntoPlay p "01017"
        card <- putInHand p "16158"
        _ <- stackEncounterDeck [Minions.taskmaster]
        threat <- mainSchemeThreatNow
        vid <- villainIdNow
        start [VillainMessage vid VillainSchemes]
        passUntil (isPlay card)
        choose "play Close Call" (isPlay card)
        pass
        -- Rhino's 1 SCH alone
        mainSchemeThreatNow `shouldReturn` (threat + 1)
        encounterDiscardCodes >>= (`shouldSatisfy` elem (toCardCode Minions.taskmaster))

    it "is not offered in alter-ego form" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putInHand p "16158"
        _ <- stackEncounterDeck [Treacheries.stampede]
        vid <- villainIdNow
        start [VillainMessage vid VillainSchemes]
        neverOffered (isPlay card)

  describe "Defy Danger (16159)" do
    it "deals 5 damage to an enemy and discards the top encounter card: 1 damage to you per boost icon" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- stackEncounterDeck [Treacheries.stampede]
        card <- putInHand p "16159"
        playPayingEnergy p card
        pass
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 5
        damageOf p `shouldReturn` 1
        encounterDiscardCodes >>= (`shouldSatisfy` elem (toCardCode Treacheries.stampede))

    it "no damage to you when the card has no boost icon" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- stackEncounterDeck [Treacheries.sonicBoom]
        card <- putInHand p "16159"
        playPayingEnergy p card
        pass
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 5
        damageOf p `shouldReturn` 0

  describe "In Harm's Way (16160)" do
    it "you take 2 damage, then thwart 5 from a scheme" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 6
        threat <- mainSchemeThreatNow
        card <- putInHand p "16160"
        playPayingEnergy p card
        pass
        chooseMainScheme
        pass
        damageOf p `shouldReturn` 2
        mainSchemeThreatNow `shouldReturn` (threat - 5)

  describe "Take the Fight to Them (16161)" do
    it "looks at the top 2 encounter cards: one discarded, the other to the bottom; draws 1" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        [stampede, mercenary] <- stackEncounterDeck [Treacheries.stampede, Minions.hydraMercenary]
        card <- putInHand p "16161"
        playWith card []
        offers (isTargetChoice mercenary) `shouldReturn` True
        choose "Stampede" (isTargetChoice stampede)
        choose "discard it" (isLabel "$label.discardIt")
        choose "the mercenary" (isTargetChoice mercenary)
        choose "to the bottom" (isLabel "$label.putOnBottomOfEncounterDeck")
        pass
        map ecCardId <$> encounterDiscardNow >>= (`shouldSatisfy` elem (ecCardId stampede))
        deck <- scenarioEncounterDeckNow
        lastMay deck `shouldBe` Just mercenary
        headMay deck `shouldSatisfy` (/= Just mercenary)
        handCount p `shouldReturn` 1

    it "the rest go on top in the order chosen" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        [a, b] <- stackEncounterDeck [Treacheries.stampede, Minions.hydraMercenary]
        card <- putInHand p "16161"
        playWith card []
        choose "the mercenary" (isTargetChoice b)
        choose "on top" (isLabel "$label.putOnTopOfEncounterDeck")
        choose "Stampede" (isTargetChoice a)
        choose "on top, beneath it" (isLabel "$label.putOnTopOfEncounterDeck")
        pass
        take 2 <$> scenarioEncounterDeckNow `shouldReturn` [b, a]

    it "looks at 2 per player" do
      tg <- testGame ["captain-marvel", "spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        stacked <- stackEncounterDeck [Treacheries.stampede, Minions.hydraMercenary, Treacheries.stampede, Minions.hydraMercenary]
        card <- putInHand p "16161"
        playWith card []
        for_ stacked \c -> offers (isTargetChoice c) `shouldReturn` True
        maybe 0 (length . snd) <$> asked `shouldReturn` 4
