module Marvel.Aspect.VenomPackSpec (spec) where

import Control.Monad (replicateM)
import Data.List (find)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Payment.Types (Payment (NoPayment))
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as V
import TestImport

venom :: IO TestGame
venom = testGame ["venom"]

-- | The player in hero form, their hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

-- | The card's own ability numbered n, not an ally's basic attack or thwart.
isAbilityN :: (IsRef a) => a -> Natural -> Choice -> Bool
isAbilityN a n = \case
  UseAbility ab -> abilitySource ab == toSource a && abilityIndex ab == n
  _ -> False

-- | Decline responses and defending until the choice is offered.
untilOffered :: (HasCallStack) => (Choice -> Bool) -> GameAppT ()
untilOffered p = go (30 :: Int)
 where
  go 0 = error "never offered"
  go n = whenM (not <$> offers p) do
    asked >>= \case
      Just (_, cs) | any isNoResponse cs -> choose "decline" isNoResponse
      Just (_, cs) | any isNoDefender cs -> choose "no defender" isNoDefender
      _ -> error "never offered"
    go (n - 1)

-- | Decline responses, the choice never offered meanwhile.
neverOffered :: (HasCallStack) => (Choice -> Bool) -> GameAppT ()
neverOffered p = go (30 :: Int)
 where
  go 0 = pure ()
  go n = do
    offers p `shouldReturn` False
    asked >>= \case
      Just (_, cs) | any isNoResponse cs -> choose "decline" isNoResponse >> go (n - 1)
      _ -> pure ()

villainAttacks :: IdentityId -> GameAppT ()
villainAttacks p = villainIdNow >>= \vid -> runNow [VillainMessage vid (VillainAttacks p)]

isAllyDefend :: AllyId -> Choice -> Bool
isAllyDefend a = \case
  AllyDefend a' _ -> a' == a
  _ -> False

chooseVillainIf :: GameAppT ()
chooseVillainIf = villainIdNow >>= chooseIf . isTargetChoice

confuseVillain :: GameAppT ()
confuseVillain = villainIdNow >>= \vid -> resolve [GiveStatus (VillainRef vid) ConfusedStatus]

villainConfused :: GameAppT Bool
villainConfused = field V.VillainConfused <$> getVillain

encounterCardsOf :: IdentityId -> GameAppT Int
encounterCardsOf p = length . (.playerIdentityEncounterCards) <$> playerAttrs p

upgradeUsesOf :: PlayerCard -> GameAppT (Maybe Natural)
upgradeUsesOf card = fmap (.upgradeUses) . find ((== upgradeOf card) . upgradeId) <$> upgradesWith (toCardCode card)

-- | Put a new copy of a player card into play, as if played for free, past the responses
-- to it: what else it asks, the test answers.
putIntoPlayNow :: IdentityId -> CardCode -> GameAppT PlayerCard
putIntoPlayNow ident code = do
  cardId <- getRandom
  let card = MkPlayerCard cardId (lookupPlayerCard code) (Just ident) (Just ident)
  runNow [PutCardIntoPlay ident card NoPayment Nothing True]
  pass
  pure card

askedToDiscardRestricted :: GameAppT Bool
askedToDiscardRestricted =
  openQuestion <&> \case
    Just (_, QuestionLabel "$label.discardARestrictedCard" _ _) -> True
    _ -> False

spec :: Spec
spec = describe "Venom pack aspect and basic cards" do
  describe "Jack Flag — Jack Harrison (20011)" do
    it "enters play with no ammo counters, so his action is not offered" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        jack <- putIntoPlay p "20011"
        (.allyCounters) <$> allyIn "20011" `shouldReturn` 0
        offers (isAbilityN (allyIdOf jack) 2) `shouldReturn` False

    it "places 1 ammo counter on himself after he thwarts" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        clearHand p
        placeMainThreat 6
        jack <- putIntoPlay p "20011"
        a <- allyIn "20011"
        choose "Jack Flag thwarts" (isAllyThwart a)
        chooseMainScheme
        choose "his response" (isAbilityN (allyIdOf jack) 1)
        pass
        (.allyCounters) <$> allyIn "20011" `shouldReturn` 1

    it "exhausts and removes an ammo counter to deal 2 damage to an enemy" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        jack <- putIntoPlay p "20011"
        resolve [PlaceCounters (toTarget (allyIdOf jack)) 1]
        choose "his action" (isAbilityN (allyIdOf jack) 2)
        chooseVillainIf
        pass
        villainDamageNow `shouldReturn` 2
        a <- allyIn "20011"
        a.allyCounters `shouldBe` 0
        a.allyExhausted `shouldBe` True

    it "his action is a Hero Action: not offered in alter-ego form" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        clearHand p
        jack <- putIntoPlay p "20011"
        resolve [PlaceCounters (toTarget (allyIdOf jack)) 1]
        offers (isAbilityN (allyIdOf jack) 2) `shouldReturn` False

  describe "Scare Tactic (20012)" do
    it "deals 3 damage to a confused enemy" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        confuseVillain
        tactic <- putInHand p "20012"
        mental <- putInHand p "01089"
        playWith tactic [mental]
        chooseVillainIf
        pass
        villainDamageNow `shouldReturn` 3

    it "is not dealt to an enemy that is not confused" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        mercenary <- addMinion p Minions.hydraMercenary
        resolve [GiveStatus (MinionRef mercenary) ConfusedStatus]
        tactic <- putInHand p "20012"
        mental <- putInHand p "01089"
        playWith tactic [mental]
        vid <- villainIdNow
        offers (isTargetChoice vid) `shouldReturn` False
        choose "the mercenary" (isTargetChoice mercenary)
        pass
        -- 3 damage defeats the mercenary's 3 HP
        minionDamageOf mercenary `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 0

    -- needs the card def's criteria to ask for a confused enemy (see report)
    it "is not played when no enemy is confused" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        tactic <- putInHand p "20012"
        _ <- putInHand p "01089"
        offers (isPlay tactic) `shouldReturn` False

  describe "Making an Entrance (20013)" do
    it "gives your hero +2 THW for the basic thwart" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        setDamage p 3
        placeMainThreat 6
        before <- mainSchemeThreatNow
        entrance <- putInHand p "20013"
        physical <- putInHand p "01090"
        choose "thwart" (isHeroThwart p)
        playWith entrance [physical]
        chooseMainScheme
        pass
        -- Venom's 1 THW and 2
        mainSchemeThreatNow `shouldReturn` (before - 3)
        -- threat is left on the main scheme: no heal
        damageOf p `shouldReturn` 3

    it "heals 2 damage from your hero after that thwart removes all threat from a scheme" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        setDamage p 3
        crowd <- sideSchemeOf <$> revealNow p SideSchemes.crowdControl
        pass
        sideSchemeThreat crowd `shouldReturn` 2
        entrance <- putInHand p "20013"
        physical <- putInHand p "01090"
        choose "thwart" (isHeroThwart p)
        playWith entrance [physical]
        choose "Crowd Control" (isTargetChoice crowd)
        pass
        damageOf p `shouldReturn` 1

    it "answers only that thwart: a later one clearing a scheme heals nothing" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        setDamage p 3
        placeMainThreat 6
        entrance <- putInHand p "20013"
        physical <- putInHand p "01090"
        choose "thwart" (isHeroThwart p)
        playWith entrance [physical]
        chooseMainScheme
        pass
        -- Crowd Control's crisis icon keeps threat on the main scheme: revealed after
        crowd <- sideSchemeOf <$> revealNow p SideSchemes.crowdControl
        pass
        for_ [1 :: Int, 2] \_ -> do
          resolve [IdentityMessage p ReadiedIdentity]
          choose "thwart" (isHeroThwart p)
          chooseIf isNoResponse
          choose "Crowd Control" (isTargetChoice crowd)
          pass
        damageOf p `shouldReturn` 3

  describe "Sonic Rifle (20015)" do
    it "exhausts and spends a charge to confuse an enemy" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        rifle <- putIntoPlay p "20015"
        choose "Sonic Rifle" (isAbilityOf (upgradeOf rifle))
        chooseVillainIf
        pass
        villainConfused `shouldReturn` True
        villainDamageNow `shouldReturn` 0
        upgradeIsExhausted (upgradeOf rifle) `shouldReturn` True
        upgradeUsesOf rifle `shouldReturn` Just 1

    it "deals 3 damage instead to an enemy already confused" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        confuseVillain
        rifle <- putIntoPlay p "20015"
        choose "Sonic Rifle" (isAbilityOf (upgradeOf rifle))
        chooseVillainIf
        pass
        villainDamageNow `shouldReturn` 3
        villainConfused `shouldReturn` True

    it "is discarded as its last charge is spent" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        rifle <- putIntoPlay p "20015"
        for_ [1 :: Int, 2] \_ -> do
          resolve [UpgradeMessage (upgradeOf rifle) ReadiedUpgrade]
          choose "Sonic Rifle" (isAbilityOf (upgradeOf rifle))
          chooseVillainIf
          pass
        upgradesWith "20015" `shouldReturn` []
        map pcCardId <$> discardOf p >>= (`shouldSatisfy` elem (pcCardId rifle))

  describe "Star-Lord — Peter Quill (20016)" do
    it "deals you a facedown encounter card as he enters play under your control" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        clearHand p
        starLord <- putInHand p "20016"
        payment <- traverse (putInHand p) ["01088", "01089"]
        before <- encounterCardsOf p
        playWith starLord payment
        choose "his forced response" (isAbilityN (allyIdOf starLord) 1)
        encounterCardsOf p `shouldReturn` (before + 1)

    -- needs the Ranged keyword on his card def (see report)
    it "attacks with ranged: a retaliating enemy deals him nothing" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "20016"
        starLord <- allyIn "20016"
        whiplash <- addMinion p Minions.whiplash
        choose "attack with Star-Lord" (isAllyAttack starLord)
        choose "Whiplash" (isTargetChoice whiplash)
        pass
        minionDamageOf whiplash `shouldReturn` Just 2
        -- his 1 consequential damage, none from Whiplash's retaliate 1
        (.allyDamage) <$> allyIn "20016" `shouldReturn` 1

  describe "Side Holster (20021)" do
    it "is played under another player's control, max 1 per player" do
      tg <- testGame ["venom", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "20021"
        holster <- putInHand p "20021"
        third <- putInHand p "20021"
        -- Venom has one, so only Spider-Man may take this
        playWith holster []
        length <$> upgradesOf p `shouldReturn` 1
        length <$> upgradesOf other `shouldReturn` 1
        -- each player has one
        offers (isPlay third) `shouldReturn` False

    it "gives its controller 1 more restricted Weapon upgrade" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "20021"
        _ <- putIntoPlayNow p "20022"
        _ <- putIntoPlayNow p "20022"
        askedToDiscardRestricted `shouldReturn` False
        _ <- putIntoPlayNow p "20015"
        askedToDiscardRestricted `shouldReturn` False
        fourth <- putIntoPlayNow p "20022"
        askedToDiscardRestricted `shouldReturn` True
        choose "discard the fourth" (isTargetChoice (upgradeIdOf fourth))
        askedToDiscardRestricted `shouldReturn` False

  describe "Plasma Pistol (20022)" do
    it "exhausts and spends a charge to deal 1 damage to an enemy" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        pistol <- putIntoPlay p "20022"
        upgradeUsesOf pistol `shouldReturn` Just 3
        choose "Plasma Pistol" (isAbilityOf (upgradeOf pistol))
        chooseVillainIf
        pass
        villainDamageNow `shouldReturn` 1
        upgradeIsExhausted (upgradeOf pistol) `shouldReturn` True
        upgradeUsesOf pistol `shouldReturn` Just 2
        offers (isAbilityOf (upgradeOf pistol)) `shouldReturn` False

    it "is discarded as its last charge is spent" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        pistol <- putIntoPlay p "20022"
        for_ [1 :: Int .. 3] \_ -> do
          resolve [UpgradeMessage (upgradeOf pistol) ReadiedUpgrade]
          choose "Plasma Pistol" (isAbilityOf (upgradeOf pistol))
          chooseVillainIf
          pass
        villainDamageNow `shouldReturn` 3
        upgradesWith "20022" `shouldReturn` []

    it "is not used in alter-ego form" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        clearHand p
        pistol <- putIntoPlay p "20022"
        offers (isAbilityOf (upgradeOf pistol)) `shouldReturn` False

  describe "Fusillade (20026)" do
    it "exhausts a Weapon upgrade you control to deal 5 damage to an enemy" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        pistol <- putIntoPlay p "20022"
        fusillade <- putInHand p "20026"
        payment <- replicateM 2 (putInHand p "01089")
        choose "play Fusillade" (isPlay fusillade)
        chooseIf (isTargetChoice (upgradeOf pistol))
        payWith payment
        chooseIf (isTargetChoice (upgradeOf pistol))
        chooseVillainIf
        pass
        villainDamageNow `shouldReturn` 5
        upgradeIsExhausted (upgradeOf pistol) `shouldReturn` True

    it "is not played without a ready Weapon upgrade" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        fusillade <- putInHand p "20026"
        _ <- replicateM 2 (putInHand p "01089")
        offers (isPlay fusillade) `shouldReturn` False
        pistol <- putIntoPlay p "20022"
        offers (isPlay fusillade) `shouldReturn` True
        choose "Plasma Pistol" (isAbilityOf (upgradeOf pistol))
        chooseVillainIf
        pass
        offers (isPlay fusillade) `shouldReturn` False

  describe "\"Welcome Aboard\" (20027)" do
    it "reduces the cost of the next ally any player plays this phase by 2, and only that one" do
      tg <- testGame ["venom", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        asHero p
        clearHand other
        welcome <- putInHand p "20027"
        playWith welcome []
        endTurn
        -- Spider-Man's turn: with no cards to pay, only a reduced ally is affordable
        starLord <- putInHand other "20016"
        maria <- putInHand other "01067"
        offers (isPlay maria) `shouldReturn` True
        playWith starLord []
        choose "his forced response" (isAbilityN (allyIdOf starLord) 1)
        pass
        length <$> alliesWith "20016" `shouldReturn` 1
        -- used up
        offers (isPlay maria) `shouldReturn` False

    it "is played once per round" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        welcome <- putInHand p "20027"
        second <- putInHand p "20027"
        playWith welcome []
        offers (isPlay second) `shouldReturn` False

    it "is played only by a Guardian identity" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        welcome <- putInHand p "20027"
        offers (isPlay welcome) `shouldReturn` False

  describe "Shake it Off (20028)" do
    it "gives a Guardian hero damaged by an attack a tough status card" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        shake <- putInHand p "20028"
        physical <- putInHand p "01090"
        -- Rhino's 2 ATK and a boost card with no icons
        _ <- stackEncounterDeck [Treacheries.hardToKeepDown]
        villainAttacks p
        untilOffered (isPlay shake)
        playWith shake [physical]
        pass
        damageOf p `shouldReturn` 2
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` True

    it "gives a Guardian ally damaged by an attack a tough status card" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        jack <- putIntoPlay p "20011"
        shake <- putInHand p "20028"
        physical <- putInHand p "01090"
        _ <- stackEncounterDeck [Treacheries.hardToKeepDown]
        villainAttacks p
        choose "Jack Flag defends" (isAllyDefend (allyIdOf jack))
        untilOffered (isPlay shake)
        playWith shake [physical]
        pass
        a <- allyIn "20011"
        a.allyDamage `shouldBe` 2
        a.allyTough `shouldBe` True

    it "is not offered for a character that is not a Guardian" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        vision <- putIntoPlay p "01068"
        shake <- putInHand p "20028"
        _ <- putInHand p "01090"
        _ <- stackEncounterDeck [Treacheries.hardToKeepDown]
        villainAttacks p
        choose "Vision defends" (isAllyDefend (allyIdOf vision))
        neverOffered (isPlay shake)

    it "is not offered when the attack deals no damage" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        shake <- putInHand p "20028"
        _ <- putInHand p "01090"
        _ <- stackEncounterDeck [Treacheries.hardToKeepDown]
        villainAttacks p
        -- Venom's 2 DEF against Rhino's 2 ATK
        choose "defend" isDefend
        neverOffered (isPlay shake)
        damageOf p `shouldReturn` 0

  describe "Crew Quarters (20029)" do
    it "is played under another player's control, max 1 per player" do
      tg <- testGame ["venom", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "20029"
        quarters <- putInHand p "20029"
        third <- putInHand p "20029"
        mental <- putInHand p "01089"
        _ <- putInHand p "01089"
        playOn quarters other [mental]
        controllers <- map (.supportController) <$> supportsWith "20029"
        controllers `shouldMatchList` [p, other]
        offers (isPlay third) `shouldReturn` False

    it "exhausts to heal 1 damage from an alter-ego, any player's" do
      tg <- testGame ["venom", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        clearHand p
        quarters <- putIntoPlay p "20029"
        setDamage other 2
        choose "Crew Quarters" (isAbilityOf (supportOf quarters))
        -- the only damaged alter-ego: chosen for the player
        chooseIf (isTargetChoice other)
        pass
        damageOf other `shouldReturn` 1
        map (.supportExhausted) <$> supportsWith "20029" `shouldReturn` [True]

    it "heals no hero" do
      tg <- testGame ["venom", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        clearHand p
        runNow [IdentityMessage other (ChangedToForm A)]
        quarters <- putIntoPlay p "20029"
        setDamage other 2
        setDamage p 2
        choose "Crew Quarters" (isAbilityOf (supportOf quarters))
        offers (isTargetChoice other) `shouldReturn` False
        chooseIf (isTargetChoice p)
        pass
        damageOf other `shouldReturn` 2
        damageOf p `shouldReturn` 1

    it "offers no undamaged alter-ego" do
      tg <- testGame ["venom", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        clearHand p
        quarters <- putIntoPlay p "20029"
        setDamage other 2
        choose "Crew Quarters" (isAbilityOf (supportOf quarters))
        offers (isTargetChoice p) `shouldReturn` False
        chooseIf (isTargetChoice other)
        pass
        damageOf other `shouldReturn` 1
        damageOf p `shouldReturn` 0

    it "is not used when no alter-ego has damage" do
      tg <- testGame ["venom", "spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        quarters <- putIntoPlay p "20029"
        offers (isAbilityOf (supportOf quarters)) `shouldReturn` False

    it "is not used in hero form" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        setDamage p 2
        quarters <- putIntoPlay p "20029"
        offers (isAbilityOf (supportOf quarters)) `shouldReturn` False
