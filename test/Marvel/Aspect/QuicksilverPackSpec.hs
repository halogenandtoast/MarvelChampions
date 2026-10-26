module Marvel.Aspect.QuicksilverPackSpec (spec) where

import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity (PlayerIdentitySide (..), currentIdentity)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as V
import TestImport

quicksilver :: IO TestGame
quicksilver = testGame ["quicksilver"]

-- | Play a card from hand paying with new copies of the cards given.
playPaying :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
playPaying p card codes = do
  payment <- traverse (putInHand p) codes
  playWith card payment

-- | Quicksilver in hero form, his hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = do
  clearHand p
  changeForm p
  clearHand p

-- | The card's own ability (its ability 1), not an ally's basic THW or ATK.
isOwnAbility :: PlayerCard -> Choice -> Bool
isOwnAbility c ch = isAbilityOfCard c ch && case ch of
  UseAbility ab -> abilityIndex ab == 1
  _ -> False

emptyDeck :: IdentityId -> GameAppT ()
emptyDeck p = withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) -> PlayerIdentity a {playerIdentityDeck = Deck []}

deckOf :: IdentityId -> GameAppT [PlayerCard]
deckOf p = unDeck . (.playerIdentityDeck) <$> playerAttrs p

villainStunned :: GameAppT Bool
villainStunned = field V.VillainStunned <$> getVillain

-- | Rhino (2 ATK) attacks the player, its boost card the one given.
villainAttacksWithBoost :: IdentityId -> CardDef -> GameAppT ()
villainAttacksWithBoost p boost = do
  _ <- stackEncounterDeck [boost]
  vid <- villainIdNow
  start [VillainMessage vid (VillainAttacks p)]

data Stats = Stats {thwart :: Natural, attack :: Natural}

heroStats :: IdentityId -> GameAppT Stats
heroStats p =
  currentIdentity <$> getPlayer p >>= \case
    HeroSide h -> Stats <$> Hero.getModifiedThwart (toAttrs h) <*> Hero.getModifiedAttack (toAttrs h)
    AlterEgoSide _ -> pure (Stats 0 0)

spec :: Spec
spec = describe "The Quicksilver pack's Protection and basic cards" do
  describe "Multiple Man (14012)" do
    it "puts a copy from hand into play, and that copy answers in turn" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        clearHand p
        emptyDeck p
        [mm1, mm2] <- traverse (putInHand p) ["14012", "14012"]
        playPaying p mm1 ["01088", "01088"]
        choose "Multiple Man" (isOwnAbility mm1)
        choose "the copy in hand" (isTargetChoice mm2)
        length <$> alliesWith "14012" `shouldReturn` 2
        handOf p >>= (`shouldSatisfy` notElem mm2)
        -- the copy put in has its own response
        offers (isOwnAbility mm2) `shouldReturn` True

    it "puts a copy from the deck into play, and each copy finds the next" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        clearHand p
        emptyDeck p
        [d] <- stackDeck p ["14012"]
        mm1 <- putInHand p "14012"
        mm2 <- putInHand p "14012"
        playPaying p mm1 ["01088", "01088"]
        choose "Multiple Man" (isOwnAbility mm1)
        choose "the copy in the deck" (isTargetChoice d)
        deckOf p >>= (`shouldSatisfy` notElem d)
        choose "the copy from the deck" (isOwnAbility d)
        choose "the copy in hand" (isTargetChoice mm2)
        choose "the last copy" (isOwnAbility mm2)
        -- none is left to find
        choose "nothing found" (isLabel "$label.noMatchingCardsFound")
        length <$> alliesWith "14012" `shouldReturn` 3

  describe "Warlock (14013)" do
    it "spends a mental resource to heal 2 damage from himself, only when damaged" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        clearHand p
        warlock <- putIntoPlay p "14013"
        genius <- putInHand p "01089"
        offers (isOwnAbility warlock) `shouldReturn` False
        w <- allyIn "14013"
        resolve [AllyMessage (allyId w) (AllyDamaged (toSource p) (toDamage 2 FromAbility))]
        (.allyDamage) <$> allyIn "14013" `shouldReturn` 2
        choose "Warlock" (isOwnAbility warlock)
        payWith [genius]
        (.allyDamage) <$> allyIn "14013" `shouldReturn` 0

    it "cannot spend another type of resource" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        clearHand p
        warlock <- putIntoPlay p "14013"
        _ <- putInHand p "01090"
        w <- allyIn "14013"
        resolve [AllyMessage (allyId w) (AllyDamaged (toSource p) (toDamage 2 FromAbility))]
        offers (isOwnAbility warlock) `shouldReturn` False

  describe "Never Back Down (14014)" do
    it "gives +2 DEF for the attack and stuns the attacker when no damage is taken" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        never <- putInHand p "14014"
        strength <- putInHand p "01090"
        -- Rhino's 2 ATK, no boost icons, against 1 DEF and 2 more
        villainAttacksWithBoost p Treacheries.imTough
        choose "defend" isDefend
        playWith never [strength]
        resolve []
        damageOf p `shouldReturn` 0
        villainStunned `shouldReturn` True

    it "does not stun when damage is taken" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        never <- putInHand p "14014"
        strength <- putInHand p "01090"
        -- 2 ATK and 2 boost against 3 DEF
        villainAttacksWithBoost p Minions.vulture
        choose "defend" isDefend
        playWith never [strength]
        resolve []
        damageOf p `shouldReturn` 1
        villainStunned `shouldReturn` False

    it "stuns when a tough status card takes the damage" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        resolve [IdentityMessage p IdentityBecomeTough]
        never <- putInHand p "14014"
        strength <- putInHand p "01090"
        villainAttacksWithBoost p Minions.vulture
        choose "defend" isDefend
        playWith never [strength]
        resolve []
        damageOf p `shouldReturn` 0
        villainStunned `shouldReturn` True

  describe "Side Step (14015)" do
    it "prevents 3 of the damage, and deals none back paid without energy" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        side <- putInHand p "14015"
        genius <- putInHand p "01089"
        vid <- villainIdNow
        start (damageIdentity p (toSource vid) (toDamage 4 FromAbility))
        playWith side [genius]
        resolve []
        damageOf p `shouldReturn` 1
        villainDamageNow `shouldReturn` 0

    it "paid with energy, deals 1 damage to the enemy the damage is from" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        side <- putInHand p "14015"
        energy <- putInHand p "01088"
        vid <- villainIdNow
        start (damageIdentity p (toSource vid) (toDamage 4 FromAbility))
        playWith side [energy]
        resolve []
        damageOf p `shouldReturn` 1
        villainDamageNow `shouldReturn` 1

    it "deals 1 damage to the attacking enemy, even with all of the damage prevented" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        side <- putInHand p "14015"
        energy <- putInHand p "01088"
        villainAttacksWithBoost p Treacheries.imTough
        choose "take the attack" isNoDefender
        playWith side [energy]
        resolve []
        damageOf p `shouldReturn` 0
        villainDamageNow `shouldReturn` 1

    -- RR "Defend, Defense": a defense-labeled ability makes its player's identity the
    -- defender of the attack, which is then not undefended
    it "makes the hero the defender of the attack it is played during" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "14007"
        side <- putInHand p "14015"
        genius <- putInHand p "01089"
        flame <- addMinion p Minions.hydraFlameSoldier
        start [MinionMessage flame (MinionAttacks p)]
        choose "take the attack" isNoDefender
        playWith side [genius]
        offers (isAbilityOf flame) `shouldReturn` False
        resolve []
        length <$> supportsWith "14007" `shouldReturn` 1

    it "an undefended attack without it is answered as undefended" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        serval <- putIntoPlay p "14007"
        flame <- addMinion p Minions.hydraFlameSoldier
        start [MinionMessage flame (MinionAttacks p)]
        choose "take the attack" isNoDefender
        choose "Hydra Flame-Soldier's forced response" (isAbilityOf flame)
        chooseIf (isTargetChoice (supportOf serval))
        resolve []
        length <$> supportsWith "14007" `shouldReturn` 0

    it "counts a wild resource only declared as energy" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        side <- putInHand p "14015"
        wild <- putInHand p "13024"
        vid <- villainIdNow
        start (damageIdentity p (toSource vid) (toDamage 4 FromAbility))
        playDeclaring side [wild] Energy
        resolve []
        villainDamageNow `shouldReturn` 1

    it "does not count a wild declared as another type" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        side <- putInHand p "14015"
        wild <- putInHand p "13024"
        vid <- villainIdNow
        start (damageIdentity p (toSource vid) (toDamage 4 FromAbility))
        playDeclaring side [wild] Mental
        resolve []
        villainDamageNow `shouldReturn` 0

    it "deals no damage when the damage is not from an enemy" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        side <- putInHand p "14015"
        energy <- putInHand p "01088"
        start (damageIdentity p (toSource p) (toDamage 4 FromAbility))
        playWith side [energy]
        resolve []
        damageOf p `shouldReturn` 1
        villainDamageNow `shouldReturn` 0

  describe "Nerves of Steel (14017)" do
    it "exhausts to pay an energy resource for a Defense event" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        nerves <- putIntoPlay p "14017"
        side <- putInHand p "14015"
        vid <- villainIdNow
        start (damageIdentity p (toSource vid) (toDamage 4 FromAbility))
        choose "play Side Step" (isPlay side)
        choose "Nerves of Steel" (isAbilityOf (upgradeIdOf nerves))
        chooseIf isFinishPayment
        resolve []
        upgradeIsExhausted (upgradeIdOf nerves) `shouldReturn` True
        damageOf p `shouldReturn` 1
        -- the resource is energy
        villainDamageNow `shouldReturn` 1

    it "does not pay for another card" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "14017"
        rush <- putInHand p "14022"
        offers (isPlay rush) `shouldReturn` False

  describe "Order and Chaos (14018)" do
    it "cancels a treachery's when revealed effects and deals 2 damage to the villain" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "14002"
        order <- putInHand p "14018"
        strength <- putInHand p "01090"
        card <- encounterCard Treacheries.imTough
        runNow [RevealEncounterCard p card]
        playWith order [strength]
        resolve []
        V.villainIsTough <$> getVillain `shouldReturn` False
        villainDamageNow `shouldReturn` 2

    it "cancels its incite too" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "14002"
        order <- putInHand p "14018"
        strength <- putInHand p "01090"
        before <- mainSchemeThreatNow
        card <- encounterCard Treacheries.huntedByHydra
        runNow [RevealEncounterCard p card]
        playWith order [strength]
        resolve []
        mainSchemeThreatNow `shouldReturn` before
        villainDamageNow `shouldReturn` 2

    it "is not played without the Scarlet Witch in play" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        order <- putInHand p "14018"
        _ <- putInHand p "01090"
        card <- encounterCard Treacheries.imTough
        runNow [RevealEncounterCard p card]
        offers (isPlay order) `shouldReturn` False

  describe "Adrenaline Rush (14022)" do
    it "is discarded to give the hero +1 ATK until the end of the phase" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        rush <- putIntoPlay p "14022"
        before <- (.attack) <$> heroStats p
        choose "Adrenaline Rush" (isOwnAbility rush)
        (.attack) <$> heroStats p `shouldReturn` before + 1
        upgradesWith "14022" `shouldReturn` []
        endTurn
        (.attack) <$> heroStats p `shouldReturn` before

  describe "Civic Duty (14023)" do
    it "is discarded to give the hero +1 THW until the end of the phase" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        duty <- putIntoPlay p "14023"
        before <- (.thwart) <$> heroStats p
        choose "Civic Duty" (isOwnAbility duty)
        (.thwart) <$> heroStats p `shouldReturn` before + 1
        upgradesWith "14023" `shouldReturn` []
        endTurn
        (.thwart) <$> heroStats p `shouldReturn` before

    it "is not used in alter-ego form" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        clearHand p
        duty <- putIntoPlay p "14023"
        offers (isOwnAbility duty) `shouldReturn` False
