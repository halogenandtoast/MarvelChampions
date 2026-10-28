module Marvel.Aspect.MadTitansShadowAspectSpec (spec) where

import Marvel.Card.Def (canonicalCardCode)
import Marvel.Event.Cards qualified as Events
import Marvel.Identity (getModifiedCost)
import Marvel.Keyword (Keyword (Retaliate, Toughness))
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

warlock :: IO TestGame
warlock = testGame ["adam-warlock"]

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass

-- | The player's deck is exactly new copies of these cards, the first on top.
setDeck :: IdentityId -> [CardCode] -> GameAppT [PlayerCard]
setDeck p codes = do
  withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) -> PlayerIdentity a {playerIdentityDeck = Deck []}
  stackDeck p codes

-- | Basic resources: Energy, Genius ([mental]), Strength ([physical]).
energy, genius, strength :: CardCode
energy = "01088"
genius = "01089"
strength = "01090"

discardCount :: Natural -> Choice -> Bool
discardCount k = isLabel (countLabel "discardTopOfDeck" k)

costOf :: IdentityId -> PlayerCard -> GameAppT Int
costOf p card = do
  pl <- getPlayer p
  getModifiedCost pl card Nothing

-- | Hail Hydra! has no boost icons.
noIcons :: CardDef
noIcons = Treacheries.hailHydra

-- | Pay for Armored Vest (1 [mental]) with the resource card, in hero form unless asked
-- otherwise: the spent card's response comes next.
spendOnVest :: IdentityId -> CardCode -> GameAppT ()
spendOnVest p code = do
  vest <- putInHand p "21063"
  res <- putInHand p code
  playWith vest [res]

spec :: Spec
spec = describe "The Mad Titan's Shadow aspect cards" do
  describe "reprints" do
    it "look up as the cards they reprint" do
      for_
        [ ("21044", "01054")
        , ("21045", "01057")
        , ("21049", "01060")
        , ("21051", "01065")
        , ("21056", "01071")
        , ("21057", "01074")
        , ("21062", "01077")
        , ("21063", "01081")
        ]
        \(new, old) -> (canonicalCardCode (lookupPlayerCard new) `shouldBe` CardCode old :: IO ())

  describe "Marvel Boy, Noh-Varr (21041)" do
    it "spends a [physical] resource as he attacks: the attack gains piercing and ranged" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "21041"
        mb <- allyIn "21041"
        str <- putInHand p strength
        whiplash <- addMinion p Minions.whiplash
        resolve [GiveStatus (MinionRef whiplash) ToughStatus]
        choose "attack" (isAllyAttack mb)
        choose "Whiplash" (isTargetChoice whiplash)
        choose "Marvel Boy's interrupt" (isAbilityOf (AllyRef $ allyId mb))
        payWith [str]
        pass
        -- the tough status card pierced, and no retaliate: only his consequential damage
        minionDamageOf whiplash `shouldReturn` Just 2
        (.allyDamage) <$> allyIn "21041" `shouldReturn` 1

    it "without it, the tough status card stops the attack and Whiplash retaliates" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "21041"
        mb <- allyIn "21041"
        _ <- putInHand p strength
        whiplash <- addMinion p Minions.whiplash
        resolve [GiveStatus (MinionRef whiplash) ToughStatus]
        choose "attack" (isAllyAttack mb)
        choose "Whiplash" (isTargetChoice whiplash)
        choose "no response" isNoResponse
        pass
        minionDamageOf whiplash `shouldReturn` Just 0
        -- 1 consequential and 1 retaliate: his 2 hit points
        alliesWith "21041" >>= (`shouldBe` []) . map allyId

    it "is not offered with no [physical] resource to spend" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "21041"
        mb <- allyIn "21041"
        choose "attack" (isAllyAttack mb)
        chooseVillain
        offers (isAbilityOf (AllyRef $ allyId mb)) `shouldReturn` False

  describe "Cosmic Entities (21042, 21048, 21054, 21060)" do
    it "played, In-Betweener is shuffled into the encounter deck, not discarded" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        clearHand p
        ib <- putInHand p "21042"
        payment <- traverse (putInHand p) [genius, genius]
        playWith ib payment
        pass
        map toCardCode <$> scenarioEncounterDeckNow >>= (`shouldSatisfy` elem "21042")
        map toCardCode <$> discardOf p >>= (`shouldSatisfy` notElem "21042")

    it "revealed, In-Betweener deals 2 damage to the villain and leaves the game" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Events.inBetweener
        pass
        villainDamageNow `shouldReturn` 2
        map toCardCode <$> encounterDiscardNow >>= (`shouldSatisfy` notElem "21042")
        map toCardCode <$> scenarioEncounterDeckNow >>= (`shouldSatisfy` notElem "21042")
        map toCardCode <$> discardOf p >>= (`shouldSatisfy` notElem "21042")

    it "revealed, it cannot be cancelled: Spycraft is not offered" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        asHero p
        clearHand p
        spycraft <- putIntoPlay p "08018"
        ib <- encounterCard Events.inBetweener
        runNow [RevealEncounterCard p ib]
        offers (isAbilityOfCard spycraft) `shouldReturn` False
        pass
        villainDamageNow `shouldReturn` 2

    it "revealed, Living Tribunal removes 2 threat from the main scheme" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        placeMainThreat 3
        before <- mainSchemeThreatNow
        _ <- revealNow p Events.livingTribunal
        chooseIf . isTargetChoice =<< mainSchemeIdNow
        pass
        mainSchemeThreatNow `shouldReturn` (before - 2)

    it "revealed, Eternity draws a card for the player revealing it" do
      tg <- testGame ["spider-man", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        mine <- length <$> handOf p
        theirs <- length <$> handOf other
        _ <- revealNow other Events.eternity
        pass
        length <$> handOf other `shouldReturn` (theirs + 1)
        length <$> handOf p `shouldReturn` mine

    it "revealed, The Gardener heals 2 damage from the revealing player's identity" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        setDamage p 3
        _ <- revealNow p Events.theGardener
        pass
        damageOf p `shouldReturn` 1

    it "dealt facedown, it resolves as the dealt cards are revealed" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        before <- length <$> handOf p
        _ <- stackEncounterDeck [Events.eternity]
        runNow [DealEncounterCard p, RevealDealtEncounterCards]
        pass
        length <$> handOf p `shouldReturn` (before + 1)
        map toCardCode <$> encounterDiscardNow >>= (`shouldSatisfy` notElem "21054")

    it "as a boost card it adds nothing and goes to the encounter discard pile" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        asHero p
        hand <- length <$> handOf p
        vid <- villainIdNow
        _ <- stackEncounterDeck [Events.eternity]
        runNow [VillainMessage vid (VillainAttacks p)]
        choose "no defender" isNoDefender
        pass
        -- Rhino's 2 ATK, no boost icons, and no "When Revealed"
        damageOf p `shouldReturn` 2
        length <$> handOf p `shouldReturn` hand
        encounterDiscardCodes >>= (`shouldSatisfy` elem "21054")

  describe "Magic Attack (21043)" do
    it "deals 1 damage for each card discarded from the top of the deck, as one attack" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        asHero p
        clearHand p
        _ <- stackDeck p [energy, energy, energy, energy, energy, energy]
        ma <- putInHand p "21043"
        str <- putInHand p strength
        playWith ma [str]
        -- up to 5, none allowed
        offers (discardCount 0) `shouldReturn` True
        offers (discardCount 5) `shouldReturn` True
        offers (discardCount 6) `shouldReturn` False
        choose "discard 3" (discardCount 3)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 3
        length . filter ((== "01088") . toCardCode) <$> discardOf p `shouldReturn` 3

    it "offers no more than the deck holds" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        asHero p
        clearHand p
        _ <- setDeck p [energy, energy]
        ma <- putInHand p "21043"
        str <- putInHand p strength
        playWith ma [str]
        offers (discardCount 2) `shouldReturn` True
        offers (discardCount 3) `shouldReturn` False

    it "is played only by a Mystic identity" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        clearHand p
        ma <- putInHand p "21043"
        _ <- putInHand p strength
        offers (isPlay ma) `shouldReturn` False

  describe "Audacity (21046)" do
    it "after you spend it, deals 1 damage to the villain" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        clearHand p
        spendOnVest p "21046"
        choose "Audacity" (isLabel (countLabel "dealDamageToTheVillain" 1))
        pass
        villainDamageNow `shouldReturn` 1

    it "is not offered in alter-ego form" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        spendOnVest p "21046"
        offers (isLabel (countLabel "dealDamageToTheVillain" 1)) `shouldReturn` False
        pass
        villainDamageNow `shouldReturn` 0

  describe "Quasar, Wendell Vaughn (21047)" do
    it "after he enters play, removes 1 threat from each scheme" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        placeMainThreat 3
        scheme <- sideSchemeOf <$> revealNow p SideSchemes.bombScare
        pass
        main' <- mainSchemeThreatNow
        side <- sideSchemeThreat scheme
        quasar <- putInHand p "21047"
        payment <- traverse (putInHand p) [energy, energy, energy]
        playWith quasar payment
        choose "Quasar" (isAbilityOfCard quasar)
        pass
        mainSchemeThreatNow `shouldReturn` (main' - 1)
        sideSchemeThreat scheme `shouldReturn` (side - 1)

    it "removes threat without thwarting: patrol does not keep it off the main scheme" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        placeMainThreat 3
        _ <- addMinion p Minions.headhuntersHenchman
        main' <- mainSchemeThreatNow
        quasar <- putInHand p "21047"
        payment <- traverse (putInHand p) [energy, energy, energy]
        playWith quasar payment
        choose "Quasar" (isAbilityOfCard quasar)
        pass
        mainSchemeThreatNow `shouldReturn` (main' - 1)

  describe "Zone of Silence (21050)" do
    it "removes 1 threat from the scheme for each card discarded from the top of the deck" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        asHero p
        clearHand p
        placeMainThreat 4
        before <- mainSchemeThreatNow
        _ <- stackDeck p [energy, energy, energy, energy, energy]
        zone <- putInHand p "21050"
        g <- putInHand p genius
        playWith zone [g]
        offers (discardCount 4) `shouldReturn` True
        offers (discardCount 5) `shouldReturn` False
        choose "discard 2" (discardCount 2)
        chooseIf . isTargetChoice =<< mainSchemeIdNow
        pass
        mainSchemeThreatNow `shouldReturn` (before - 2)

  describe "Determination (21052)" do
    it "after you spend it, removes 1 threat from the main scheme" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        clearHand p
        placeMainThreat 3
        before <- mainSchemeThreatNow
        spendOnVest p "21052"
        choose "Determination" (isLabel (countLabel "removeThreatFromMainScheme" 1))
        chooseIf . isTargetChoice =<< mainSchemeIdNow
        pass
        mainSchemeThreatNow `shouldReturn` (before - 1)

  describe "Major Victory, Vance Astro (21053)" do
    it "as he is defeated, readies an exhausted friendly Guardian character" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "21053"
        mv <- allyIn "21053"
        _ <- putIntoPlay p "21059"
        charlie <- allyIn "21059"
        resolve [AllyMessage (allyId charlie) ExhaustedAlly]
        start [AllyMessage (allyId mv) AllyDefeated]
        choose "Major Victory" (isAbilityOf (AllyRef $ allyId mv))
        choose "Charlie-27" (isTargetChoice (AllyRef $ allyId charlie))
        pass
        (.allyExhausted) <$> allyIn "21059" `shouldReturn` False

    it "readies an exhausted Guardian hero" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "21053"
        mv <- allyIn "21053"
        resolve [IdentityMessage p ExhaustedIdentity]
        start [AllyMessage (allyId mv) AllyDefeated]
        choose "Major Victory" (isAbilityOf (AllyRef $ allyId mv))
        choose "Adam Warlock" (isTargetChoice (IdentityRef p))
        pass
        exhaustedNow p `shouldReturn` False

    it "is not offered with no exhausted Guardian character" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "21053"
        mv <- allyIn "21053"
        -- Spider-Man is no Guardian, and Charlie-27 is ready
        resolve [IdentityMessage p ExhaustedIdentity]
        _ <- putIntoPlay p "21059"
        start [AllyMessage (allyId mv) AllyDefeated]
        offers (isAbilityOf (AllyRef $ allyId mv)) `shouldReturn` False

  describe "Summoning Spell (21055)" do
    it "discards until an ally and puts it into play" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        asHero p
        clearHand p
        _ <- stackDeck p [energy, "21059"]
        spell <- putInHand p "21055"
        payment <- traverse (putInHand p) [genius, genius]
        playWith spell payment
        pass
        map allyController <$> alliesWith "21059" `shouldReturn` [p]
        map toCardCode <$> discardOf p >>= (`shouldSatisfy` elem "01088")

  describe "Innovation (21058)" do
    it "after you spend it, heals 1 damage from an ally you control" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        clearHand p
        _ <- putIntoPlay p "21053"
        mv <- allyIn "21053"
        resolve [AllyMessage (allyId mv) (AllyDamaged (toSource p) (toDamage 1 FromAbility))]
        spendOnVest p "21058"
        choose "Major Victory" (isTargetChoice (AllyRef $ allyId mv))
        pass
        (.allyDamage) <$> allyIn "21053" `shouldReturn` 0

  describe "Charlie-27 (21059)" do
    it "has retaliate 1 and toughness" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        let keywords = cdKeywords (lookupPlayerCard "21059")
        keywords `shouldSatisfy` elem (Retaliate 1)
        keywords `shouldSatisfy` elem Toughness
        _ <- putIntoPlay p "21059"
        (.allyTough) <$> allyIn "21059" `shouldReturn` True

  describe "Shield Spell (21061)" do
    it "discards as many cards as the attack's damage, preventing all of it" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        asHero p
        clearHand p
        _ <- stackDeck p [energy, energy, energy]
        ss <- putInHand p "21061"
        vid <- villainIdNow
        _ <- stackEncounterDeck [noIcons]
        runNow [VillainMessage vid (VillainAttacks p)]
        choose "no defender" isNoDefender
        choose "Shield Spell" (isPlay ss)
        payWith []
        pass
        damageOf p `shouldReturn` 0
        length . filter ((== "01088") . toCardCode) <$> discardOf p `shouldReturn` 2

    it "is not playable with fewer cards in the deck than the damage" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        asHero p
        clearHand p
        _ <- setDeck p [energy]
        ss <- putInHand p "21061"
        vid <- villainIdNow
        _ <- stackEncounterDeck [noIcons]
        runNow [VillainMessage vid (VillainAttacks p)]
        choose "no defender" isNoDefender
        offers (isPlay ss) `shouldReturn` False

  describe "Preservation (21064)" do
    it "after you spend it, heals 1 damage from your hero" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        clearHand p
        setDamage p 3
        spendOnVest p "21064"
        choose "Preservation" (isLabel (countLabel "healYourIdentity" 1))
        pass
        damageOf p `shouldReturn` 2

  describe "Martinex, T'Naga (21065)" do
    it "costs 1 less while your identity has the Guardian trait" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        clearHand p
        -- Adam Warlock's alter-ego is Mystic only
        martinex <- putInHand p "21065"
        costOf p martinex `shouldReturn` 3
        asHero p
        martinex' <- putInHand p "21065"
        costOf p martinex' `shouldReturn` 2
