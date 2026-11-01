module Marvel.Hero.AdamWarlockSpec (spec) where

import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity ( currentIdentity )
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types (villainIsTough)
import TestImport

warlock :: IO TestGame
warlock = testGame ["adam-warlock"]

energy, genius, strength :: CardCode
energy = "01088"
genius = "01089"
strength = "01090"

-- | Adam Warlock, his hand empty and his turn's flip spent.
asWarlock :: IdentityId -> GameAppT ()
asWarlock p = changeForm p >> pass >> clearHand p

-- | Battle Mage, discarding this card.
battleMage :: (HasCallStack) => PlayerCard -> GameAppT ()
battleMage card = do
  choose "Battle Mage" (isNamedAbility "Battle Mage")
  choose "discard" (isTargetChoice card)

data Stats = Stats {thwart :: Natural, attack :: Natural, defense :: Natural}
  deriving stock (Show, Eq)

heroStats :: IdentityId -> GameAppT Stats
heroStats p =
  currentIdentity <$> getPlayer p >>= \case
    HeroSide h -> Stats <$> Hero.getModifiedThwart (toAttrs h) <*> Hero.getModifiedAttack (toAttrs h) <*> Hero.getModifiedDefense (toAttrs h)
    AlterEgoSide _ -> pure (Stats 0 0 0)

-- | The ability Pip the Troll has in hand.
isPipInHand :: PlayerCard -> Choice -> Bool
isPipInHand pip = \case
  UseAbility ab -> abilitySource ab == CardIdRef (pcCardId pip)
  _ -> False

-- | Rhino attacks the player, his boost card adding nothing.
rhinoAttacks :: IdentityId -> GameAppT ()
rhinoAttacks p = do
  _ <- stackEncounterDeck [Treacheries.imTough]
  vid <- villainIdNow
  runNow [VillainMessage vid (VillainAttacks p)]

-- | The alter-ego's action, removing a status card.
isAlterEgoAction :: IdentityId -> Choice -> Bool
isAlterEgoAction p = \case
  UseAbility ab -> abilitySource ab == toSource p && abilityIndex ab == 1 && ChangeForm `notElem` abilityChoices ab
  _ -> False

-- | Empty the player's deck by discarding it, so it is reset from the discard pile.
runOutOfCards :: IdentityId -> GameAppT ()
runOutOfCards p = runNow [IdentityMessage p (DiscardFrom FromDeck 100 Nothing)]

spec :: Spec
spec = describe "Adam Warlock" do
  describe "Avatar of Life (21031b)" do
    let d = starter "adam-warlock"
        without c = d {cards = filter ((/= CardCode c) . fst) d.cards}
    it "accepts his starter deck: 40 cards, six of each aspect, one copy of each" $ liftIO @IO do
      sum (map snd d.cards) `shouldBe` 40
      deckBuildingError d `shouldBe` Nothing
    it "refuses a second copy of a card not his own, by either printing" $ liftIO @IO do
      deckBuildingError d {cards = (CardCode "21044", 1) : d.cards} `shouldBe` Just "error.avatarOfLife"
      deckBuildingError d {cards = (CardCode "01054", 1) : d.cards} `shouldBe` Just "error.avatarOfLife"
    it "refuses unequal numbers of the four aspects" $ liftIO @IO do
      deckBuildingError (without "21041") `shouldBe` Just "error.avatarOfLife"
    it "takes more copies of his own cards" $ liftIO @IO do
      deckBuildingError d {cards = (CardCode "21038", 1) : d.cards} `shouldBe` Nothing
    it "leaves Jessica Drew's rule to her" $ liftIO @IO do
      deckBuildingError (starter "spider-woman") {aspects = ["aggression"]} `shouldBe` Just "error.unbalancedAspects"
      deckBuildingError (starter "hawkeye") `shouldBe` Nothing

  describe "Battle Mage (21031a)" do
    it "discarding an Aggression card deals 2 damage to an enemy, once per phase" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        asWarlock p
        uppercut <- putInHand p "21044"
        _ <- putInHand p energy
        battleMage uppercut
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 2
        discardOf p >>= (`shouldSatisfy` elem uppercut)
        offers (isNamedAbility "Battle Mage") `shouldReturn` False

    it "discarding a Justice card removes 2 threat from a scheme" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        asWarlock p
        placeMainThreat 4
        before <- mainSchemeThreatNow
        card <- putInHand p "21049"
        battleMage card
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` before - 2

    it "discarding a Protection card heals 1 damage from any player's ally" do
      tg <- testGame ["adam-warlock", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        asWarlock p
        cat <- putIntoPlay other "01020"
        resolve [AllyMessage (allyIdOf cat) (AllyDamaged GameRef (toDamage 2 FromAbility))]
        card <- putInHand p "21062"
        battleMage card
        choose "the ally" (isTargetChoice (allyIdOf cat))
        pass
        (.allyDamage) <$> allyIn "01020" `shouldReturn` 1

    it "discarding a Leadership card gives a hero +1 THW, +1 ATK and +1 DEF this round" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        asWarlock p
        card <- putInHand p "21056"
        battleMage card
        choose "the hero" (isTargetChoice p)
        pass
        heroStats p `shouldReturn` Stats 2 2 3
        resolve [EndRound]
        heroStats p `shouldReturn` Stats 1 1 2

    it "discarding a basic card does nothing, but has resolved Battle Mage" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        asWarlock p
        senses <- putIntoPlay p "21037"
        _ <- stackDeck p [energy]
        card <- putInHand p energy
        battleMage card
        villainDamageNow `shouldReturn` 0
        choose "Mystic Senses" (isAbilityOfCard senses)
        length <$> handOf p `shouldReturn` 1

  describe "Avatar of Life's action (21031b)" do
    it "discards a card to remove a status card from him" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putInHand p energy
        offers (isAlterEgoAction p) `shouldReturn` False
        resolve [IdentityMessage p IdentityConfused]
        card <- putInHand p genius
        choose "the action" (isAlterEgoAction p)
        choose "discard" (isTargetChoice card)
        choose "confused" (isLabel $ statusLabel ConfusedStatus)
        isConfused p `shouldReturn` False

  describe "Pip the Troll (21032)" do
    it "from his owner's hand, enters play under the player attacked, paid with [energy][mental]" do
      tg <- testGame ["adam-warlock", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        clearHand p
        pip <- putInHand p "21032"
        payment <- traverse (putInHand p) [energy, genius]
        rhinoAttacks other
        choose "Pip" (isPipInHand pip)
        offers (isPayWith pip) `shouldReturn` False
        payWith payment
        pips <- alliesWith "21032"
        map allyController pips `shouldBe` [other]
        handOf p `shouldReturn` []
        let pipDefends = \case
              AllyDefend a _ -> a == allyIdOf pip
              _ -> False
        offers pipDefends `shouldReturn` True

    it "is not paid for with himself" do
      tg <- testGame ["adam-warlock", "spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        pip <- putInHand p "21032"
        _ <- putInHand p genius
        rhinoAttacks (player2 tg)
        offers (isPipInHand pip) `shouldReturn` False

    it "in play, has no such ability" do
      tg <- testGame ["adam-warlock", "spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        pip <- putIntoPlay p "21032"
        _ <- traverse (putInHand p) [energy, genius]
        rhinoAttacks (player2 tg)
        offers (isPipInHand pip) `shouldReturn` False

  describe "Soul World (21033)" do
    it "gains a soul counter as the deck resets, and spends it to heal all damage" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        soul <- putIntoPlay p "21033"
        runOutOfCards p
        choose "Soul World" (isAbilityOfCard soul)
        pass
        map supportUses <$> supportsWith "21033" `shouldReturn` [1]
        setDamage p 6
        choose "heal" (isAbilityOfCard soul)
        pass
        damageOf p `shouldReturn` 0
        map supportUses <$> supportsWith "21033" `shouldReturn` [0]
        offers (isAbilityOfCard soul) `shouldReturn` False

  describe "Karmic Staff (21034)" do
    it "exhausts for a wild resource" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        staff <- putIntoPlay p "21034"
        clearHand p
        magic <- putInHand p "21040"
        _ <- putInDiscard p energy
        choose "play Quantum Magic" (isPlay magic)
        choose "pay with the staff" (isAbilityOfCard staff)
        map upgradeExhausted <$> upgradesWith "21034" `shouldReturn` [True]

  describe "Warlock's Cape (21035)" do
    it "readies Adam Warlock after Battle Mage" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        asWarlock p
        cape <- putIntoPlay p "21035"
        resolve [IdentityMessage p ExhaustedIdentity]
        card <- putInHand p energy
        battleMage card
        choose "the cape" (isAbilityOfCard cape)
        isExhaustedNow p `shouldReturn` False
        map upgradeExhausted <$> upgradesWith "21035" `shouldReturn` [True]

  describe "Mystic Senses (21037)" do
    it "after Battle Mage discards an aspect card, draws 1 card" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        asWarlock p
        senses <- putIntoPlay p "21037"
        _ <- stackDeck p [energy]
        card <- putInHand p "21044"
        battleMage card
        vid <- villainIdNow
        chooseIf (isTargetChoice vid)
        villainDamageNow `shouldReturn` 2
        choose "Mystic Senses" (isAbilityOfCard senses)
        map toCardCode <$> handOf p `shouldReturn` ["01088"]

    it "does not answer a basic attack" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        asWarlock p
        senses <- putIntoPlay p "21037"
        vid <- villainIdNow
        choose "attack" (isHeroAttack p)
        choose "the villain" (isTargetChoice vid)
        offers (isAbilityOfCard senses) `shouldReturn` False

  describe "Cosmic Ward (21036)" do
    it "cancels a treachery you reveal and discards it, then itself" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        ward <- putIntoPlay p "21036"
        _ <- revealNow p Treacheries.imTough
        chooseIf (isAbilityOfCard ward)
        pass
        villainIsTough <$> getVillain `shouldReturn` False
        encounterDiscardCodes >>= (`shouldSatisfy` elem "01105")
        upgradesWith "21036" `shouldReturn` []
        discardOf p >>= (`shouldSatisfy` elem ward)

    it "does not answer another player's treachery" do
      tg <- testGame ["adam-warlock", "spider-man"]
      let p = player1 tg
      inGame tg do
        ward <- putIntoPlay p "21036"
        _ <- revealNow (player2 tg) Treacheries.imTough
        offers (isAbilityOfCard ward) `shouldReturn` False
        pass
        length <$> upgradesWith "21036" `shouldReturn` 1

  describe "Karmic Blast (21038)" do
    it "deals 4 damage and 1 more for each different aspect discarded, as one attack" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        asWarlock p
        _ <- stackDeck p ["21044", "21041", "21049", energy]
        blast <- putInHand p "21038"
        pay <- putInHand p strength
        playWith blast [pay]
        choose "discard 4" (isLabel $ countLabel "discardTopOfDeck" 4)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 6
        length <$> discardOf p `shouldReturn` 6

    it "may discard none" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        asWarlock p
        blast <- putInHand p "21038"
        pay <- putInHand p strength
        playWith blast [pay]
        choose "discard none" (isLabel $ countLabel "discardTopOfDeck" 0)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 4

  describe "Cosmic Awareness (21039)" do
    it "removes 3 threat and 1 more for each different aspect discarded" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        asWarlock p
        placeMainThreat 6
        before <- mainSchemeThreatNow
        _ <- stackDeck p ["21044", "21049", "21056", energy]
        card <- putInHand p "21039"
        pay <- putInHand p genius
        playWith card [pay]
        choose "discard 4" (isLabel $ countLabel "discardTopOfDeck" 4)
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` before - 6

  describe "Quantum Magic (21040)" do
    it "adds a card from your discard pile to your hand, and needs one there" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        clearHand p
        magic <- putInHand p "21040"
        pay <- putInHand p energy
        discardOf p `shouldReturn` []
        offers (isPlay magic) `shouldReturn` False
        target <- putInDiscard p "21044"
        playWith magic [pay]
        chooseIf (isTargetChoice target)
        handOf p >>= (`shouldSatisfy` elem target)

  describe "Regeneration Cycle (21066)" do
    let obligation = Obligations.regenerationCycle
    it "exhausting the alter-ego removes it from the game" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        _ <- revealNow p obligation
        choose "exhaust" (isLabel "$label.exhaustAdamWarlockRemoveRegenerationCycle")
        isExhaustedNow p `shouldReturn` True
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "21066")

    it "otherwise discards 5 cards, 1 threat on the main scheme for each different aspect" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        before <- mainSchemeThreatNow
        _ <- stackDeck p ["21044", "21041", "21049", "21056", energy]
        _ <- revealNow p obligation
        choose "discard" (isLabel "$label.discardTopFiveThreatPerAspect")
        pass
        mainSchemeThreatNow `shouldReturn` before + 3
        obligationsInPlay `shouldReturn` 0

    it "a hero may flip to alter-ego first; staying a hero leaves only the discard" do
      tg <- warlock
      let p = player1 tg
      inGame tg do
        asWarlock p
        deck <- length . unDeck . (.playerIdentityDeck) <$> playerAttrs p
        _ <- revealNow p obligation
        offers (isLabel "$label.changeToB") `shouldReturn` True
        choose "stay a hero" (isLabel "$label.doNotFlipToAlterEgo")
        pass
        length . unDeck . (.playerIdentityDeck) <$> playerAttrs p `shouldReturn` deck - 5
