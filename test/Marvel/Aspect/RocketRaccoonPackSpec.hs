module Marvel.Aspect.RocketRaccoonPackSpec (spec) where

import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

-- | Rocket Raccoon (ATK 1; a Guardian as a hero, not as an alter-ego).
rocket :: IO TestGame
rocket = testGame ["rocket-raccoon"]

-- | The player in hero form, their hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

-- | Play a card from hand paying with new copies of the cards given.
playPaying :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
playPaying p card codes = traverse (putInHand p) codes >>= playWith card

pickVillain :: GameAppT ()
pickVillain = chooseIf . isTargetChoice =<< villainIdNow

pickMainScheme :: GameAppT ()
pickMainScheme = chooseIf . isTargetChoice =<< mainSchemeIdNow

-- | Decline responses until the choice is offered.
untilOffered :: (HasCallStack) => (Choice -> Bool) -> GameAppT ()
untilOffered f = go (20 :: Int)
 where
  go 0 = error "never offered"
  go n = whenM (not <$> offers f) do
    choose "decline" isNoResponse
    go (n - 1)

-- | Decline every response, failing if a matching choice is ever offered.
neverOffered :: (HasCallStack) => (Choice -> Bool) -> GameAppT ()
neverOffered f = do
  offers f `shouldReturn` False
  whenM (offers isNoResponse) $ choose "decline" isNoResponse >> neverOffered f

-- | Rhino attacks the player, the boost card on top of the encounter deck.
rhinoAttacks :: IdentityId -> CardDef -> GameAppT ()
rhinoAttacks p boost = do
  _ <- stackEncounterDeck [boost]
  vid <- villainIdNow
  start [VillainMessage vid (VillainAttacks p)]

isAllyDefending :: PlayerCard -> Choice -> Bool
isAllyDefending c = \case
  AllyDefend a _ -> a == allyIdOf c
  _ -> False

-- | The ally's own ability (index 1), not its basic powers.
isResponseOf :: PlayerCard -> Choice -> Bool
isResponseOf c ch = isAbilityOfCard c ch && case ch of
  UseAbility ab -> abilityIndex ab == 1
  _ -> False

woundAlly :: PlayerCard -> Natural -> GameAppT ()
woundAlly c n = resolve [AllyMessage (allyIdOf c) (AllyDamaged GameRef (toDamage n FromAbility))]

damageMinion :: IdentityId -> MinionId -> Natural -> GameAppT ()
damageMinion p m n = resolve [MinionMessage m (MinionDamaged (toSource p) (toDamage n FromAbility))]

emptyDeck :: IdentityId -> GameAppT ()
emptyDeck p = withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
  PlayerIdentity a {playerIdentityDeck = Deck []}

spec :: Spec
spec = describe "Rocket Raccoon pack aspect and basic cards" do
  describe "Bug (16040)" do
    it "heals 1 damage from Bug after your hero makes a basic attack" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        bug <- putIntoPlay p "16040"
        woundAlly bug 1
        choose "attack" (isHeroAttack p)
        pickVillain
        untilOffered (isResponseOf bug)
        choose "Bug" (isResponseOf bug)
        pass
        villainDamageNow `shouldReturn` 1
        (.allyDamage) <$> allyIn "16040" `shouldReturn` 0

    it "is not offered after a basic thwart" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 3
        bug <- putIntoPlay p "16040"
        woundAlly bug 1
        choose "thwart" (isHeroThwart p)
        pickMainScheme
        neverOffered (isResponseOf bug)
        (.allyDamage) <$> allyIn "16040" `shouldReturn` 1

    it "is not offered after Bug's own attack" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        bug <- putIntoPlay p "16040"
        attrs <- allyIn "16040"
        choose "Bug attacks" (isAllyAttack attrs)
        pickVillain
        neverOffered (isResponseOf bug)
        -- the consequential damage stays
        (.allyDamage) <$> allyIn "16040" `shouldReturn` 1

    it "is not offered while Bug has no damage" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        bug <- putIntoPlay p "16040"
        choose "attack" (isHeroAttack p)
        pickVillain
        neverOffered (isResponseOf bug)

  describe "Looking for Trouble (16043)" do
    it "puts the first minion of the encounter deck into play engaged with you, then removes 3 threat from the main scheme" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 5
        before <- mainSchemeThreat
        [_, mercenary] <- stackEncounterDeck [Treacheries.advance, Minions.hydraMercenary]
        trouble <- putInHand p "16043"
        choose "play" (isPlay trouble)
        pass
        pickMainScheme
        pass
        mainSchemeThreat `shouldReturn` (before - 3)
        engaged <- (.playerIdentityMinions) <$> playerAttrs p
        toList engaged `shouldBe` [minionOf mercenary]
        map toCardCode <$> encounterDiscardNow >>= (`shouldSatisfy` elem (toCardCode Treacheries.advance))

    it "is a hero action: not playable in alter-ego form" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        clearHand p
        trouble <- putInHand p "16043"
        offers (isPlay trouble) `shouldReturn` False

  describe "Follow Through (16045)" do
    it "increases the excess damage of a hero attack's overkill by 1" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        follow <- putIntoPlay p "16045"
        mercenary <- addMinion p Minions.hydraMercenary
        pass
        assault <- putInHand p "01053"
        playPaying p assault ["01090"]
        choose "the mercenary" (isTargetChoice mercenary)
        untilOffered (isAbilityOfCard follow)
        choose "Follow Through" (isAbilityOfCard follow)
        pass
        minionDamageOf mercenary `shouldReturn` Nothing
        -- 5 damage to 3 hit points: 2 excess, and 1 more
        villainDamageNow `shouldReturn` 3

    it "makes Into the Fray remove 1 more threat" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        follow <- putIntoPlay p "16045"
        placeMainThreat 5
        before <- mainSchemeThreatNow
        mercenary <- addMinion p Minions.hydraMercenary
        pass
        fray <- putInHand p "13013"
        playPaying p fray ["01088", "01088"]
        choose "the mercenary" (isTargetChoice mercenary)
        untilOffered (isAbilityOfCard follow)
        choose "Follow Through" (isAbilityOfCard follow)
        pass
        pickMainScheme
        pass
        -- 6 damage to 3 hit points: 3 excess, and 1 more
        mainSchemeThreatNow `shouldReturn` before - 4

    it "lets each copy add 1 to the same excess" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        follow1 <- putIntoPlay p "16045"
        follow2 <- putIntoPlay p "16045"
        mercenary <- addMinion p Minions.hydraMercenary
        pass
        assault <- putInHand p "01053"
        playPaying p assault ["01090"]
        choose "the mercenary" (isTargetChoice mercenary)
        untilOffered (isAbilityOfCard follow1)
        choose "Follow Through" (isAbilityOfCard follow1)
        untilOffered (isAbilityOfCard follow2)
        choose "Follow Through" (isAbilityOfCard follow2)
        neverOffered (isAbilityOfCard follow1)
        pass
        villainDamageNow `shouldReturn` 4

    it "is not offered when the attack defeats a minion with no excess" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        follow <- putIntoPlay p "16045"
        mercenary <- addMinion p Minions.hydraMercenary
        pass
        damageMinion p mercenary 2
        choose "attack" (isHeroAttack p)
        choose "the mercenary" (isTargetChoice mercenary)
        neverOffered (isAbilityOfCard follow)
        minionDamageOf mercenary `shouldReturn` Nothing

    it "is not offered for an ally's attack" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        follow <- putIntoPlay p "16045"
        _ <- putIntoPlay p "16047"
        groot <- allyIn "16047"
        mercenary <- addMinion p Minions.hydraMercenary
        pass
        damageMinion p mercenary 2
        -- Groot's 2 ATK to 1 hit point remaining
        choose "Groot attacks" (isAllyAttack groot)
        choose "the mercenary" (isTargetChoice mercenary)
        neverOffered (isAbilityOfCard follow)
        minionDamageOf mercenary `shouldReturn` Nothing

  describe "Hand Cannon (16046)" do
    it "exhausts and removes a charge counter for +2 ATK, and the attack gains overkill" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        cannon <- putIntoPlay p "16046"
        map (.upgradeUses) <$> upgradesWith "16046" `shouldReturn` [3]
        mercenary <- addMinion p Minions.hydraMercenary
        pass
        damageMinion p mercenary 1
        choose "attack" (isHeroAttack p)
        -- the interrupt comes before the target is chosen
        choose "Hand Cannon" (isAbilityOfCard cannon)
        choose "the mercenary" (isTargetChoice mercenary)
        pass
        minionDamageOf mercenary `shouldReturn` Nothing
        -- 3 damage to 2 hit points remaining: 1 goes on to Rhino
        villainDamageNow `shouldReturn` 1
        map (.upgradeUses) <$> upgradesWith "16046" `shouldReturn` [2]
        upgradeIsExhausted (upgradeOf cannon) `shouldReturn` True

    it "is discarded after its third charge counter" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        cannon <- putIntoPlay p "16046"
        for_ [1 :: Natural .. 3] \n -> do
          resolve [IdentityMessage p ReadiedIdentity, UpgradeMessage (upgradeOf cannon) ReadiedUpgrade]
          choose "attack" (isHeroAttack p)
          choose "Hand Cannon" (isAbilityOfCard cannon)
          pickVillain
          pass
          villainDamageNow `shouldReturn` (3 * n)
        upgradesWith "16046" `shouldReturn` []
        map toCardCode <$> discardOf p >>= (`shouldSatisfy` elem "16046")

    it "its last counter discards it, and that attack still gains overkill" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        cannon <- putIntoPlay p "16046"
        for_ [1 :: Int, 2] \_ -> do
          resolve [IdentityMessage p ReadiedIdentity, UpgradeMessage (upgradeOf cannon) ReadiedUpgrade]
          choose "attack" (isHeroAttack p)
          choose "Hand Cannon" (isAbilityOfCard cannon)
          pickVillain
          pass
        resolve [IdentityMessage p ReadiedIdentity, UpgradeMessage (upgradeOf cannon) ReadiedUpgrade]
        before <- villainDamageNow
        mercenary <- addMinion p Minions.hydraMercenary
        pass
        damageMinion p mercenary 1
        choose "attack" (isHeroAttack p)
        choose "Hand Cannon" (isAbilityOfCard cannon)
        choose "the mercenary" (isTargetChoice mercenary)
        pass
        upgradesWith "16046" `shouldReturn` []
        minionDamageOf mercenary `shouldReturn` Nothing
        villainDamageNow `shouldReturn` (before + 1)

    it "is not offered while exhausted" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        cannon <- putIntoPlay p "16046"
        resolve [UpgradeMessage (upgradeOf cannon) ExhaustedUpgrade]
        choose "attack" (isHeroAttack p)
        pickVillain
        neverOffered (isAbilityOfCard cannon)
        villainDamageNow `shouldReturn` 1

  describe "Groot (16047)" do
    it "heals 2 damage from himself after defending against an attack" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        groot <- putIntoPlay p "16047"
        woundAlly groot 1
        rhinoAttacks p Treacheries.advance
        choose "Groot defends" (isAllyDefending groot)
        untilOffered (isResponseOf groot)
        -- Rhino's 2 ATK, no boost
        (.allyDamage) <$> allyIn "16047" `shouldReturn` 3
        choose "Groot" (isResponseOf groot)
        pass
        (.allyDamage) <$> allyIn "16047" `shouldReturn` 1
        damageOf p `shouldReturn` 0

    it "is not offered when the attack defeats him" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        groot <- putIntoPlay p "16047"
        woundAlly groot 5
        rhinoAttacks p Treacheries.advance
        choose "Groot defends" (isAllyDefending groot)
        neverOffered (isResponseOf groot)
        alliesWith "16047" `shouldReturn` []

    it "is not offered when he does not defend" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        groot <- putIntoPlay p "16047"
        woundAlly groot 1
        rhinoAttacks p Treacheries.advance
        choose "no defender" isNoDefender
        neverOffered (isResponseOf groot)
        (.allyDamage) <$> allyIn "16047" `shouldReturn` 1

    it "can be played only by a Guardian identity" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        clearHand p
        groot <- putInHand p "16047"
        _ <- putInHand p "01088"
        _ <- putInHand p "01089"
        -- Rocket's alter-ego is no Guardian
        offers (isPlay groot) `shouldReturn` False
        changeForm p >> pass
        offers (isPlay groot) `shouldReturn` True

    it "cannot be played by Captain Marvel" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        groot <- putInHand p "16047"
        _ <- putInHand p "01088"
        _ <- putInHand p "01089"
        offers (isPlay groot) `shouldReturn` False

  describe "Booster Boots (16052)" do
    it "exhausts and discards the top card of your deck to prevent 1 of an attack's damage" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        boots <- putIntoPlay p "16052"
        [top] <- stackDeck p ["01088"]
        rhinoAttacks p Treacheries.stampede
        choose "no defender" isNoDefender
        untilOffered (isAbilityOfCard boots)
        choose "Booster Boots" (isAbilityOfCard boots)
        pass
        -- Rhino's 2 ATK and Stampede's 1, 1 prevented
        damageOf p `shouldReturn` 2
        upgradeIsExhausted (upgradeOf boots) `shouldReturn` True
        discardOf p >>= (`shouldSatisfy` elem top)

    it "cannot be used with an empty deck" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        boots <- putIntoPlay p "16052"
        emptyDeck p
        rhinoAttacks p Treacheries.stampede
        choose "no defender" isNoDefender
        neverOffered (isAbilityOfCard boots)
        damageOf p `shouldReturn` 3

    it "does not answer damage that is not an attack's" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        boots <- putIntoPlay p "16052"
        runNow (damageIdentity p (IdentityRef p) (toDamage 2 FromAbility))
        neverOffered (isAbilityOfCard boots)
        damageOf p `shouldReturn` 2

    it "can be played only by a Guardian identity, max 1 per player" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        clearHand p
        boots <- putInHand p "16052"
        boots2 <- putInHand p "16052"
        energy <- putInHand p "01088"
        _ <- putInHand p "01088"
        offers (isPlay boots) `shouldReturn` False
        changeForm p >> pass
        playWith boots [energy]
        pass
        length <$> upgradesWith "16052" `shouldReturn` 1
        offers (isPlay boots2) `shouldReturn` False

    it "cannot be played by Captain Marvel" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        boots <- putInHand p "16052"
        _ <- putInHand p "01088"
        offers (isPlay boots) `shouldReturn` False

  describe "reprints" do
    it "Chase Them Down (16041) plays as 01052: 2 threat removed after defeating an enemy" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        chase <- putInHand p "16041"
        placeMainThreat 3
        threat <- mainSchemeThreatNow
        mercenary <- addMinion p Minions.hydraMercenary
        pass
        damageMinion p mercenary 2
        choose "attack" (isHeroAttack p)
        choose "the mercenary" (isTargetChoice mercenary)
        untilOffered (isPlay chase)
        choose "play Chase Them Down" (isPlay chase)
        pickMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` (threat - 2)

    it "Into the Fray (16042) plays as 13013: a threat removed for each excess damage" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 5
        before <- mainSchemeThreatNow
        mercenary <- addMinion p Minions.hydraMercenary
        pass
        fray <- putInHand p "16042"
        playPaying p fray ["01088", "01088"]
        choose "the mercenary" (isTargetChoice mercenary)
        pass
        pickMainScheme
        pass
        minionDamageOf mercenary `shouldReturn` Nothing
        mainSchemeThreatNow `shouldReturn` before - 3

    it "Relentless Assault (16044) plays as 01053: 5 damage to a minion, overkill paid with physical" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        mercenary <- addMinion p Minions.hydraMercenary
        pass
        assault <- putInHand p "16044"
        playPaying p assault ["01090"]
        choose "the mercenary" (isTargetChoice mercenary)
        pass
        minionDamageOf mercenary `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 2

    it "Energy (16049) plays as 01088: 2 resources" $ paysTwo "16049"
    it "Genius (16050) plays as 01089: 2 resources" $ paysTwo "16050"
    it "Strength (16051) plays as 01090: 2 resources" $ paysTwo "16051"

-- | Clobber (cost 2, 3 damage) paid with the one resource card.
paysTwo :: CardCode -> IO ()
paysTwo code = do
  tg <- rocket
  let p = player1 tg
  inGame tg do
    asHero p
    clobber <- putInHand p "18012"
    resource <- putInHand p code
    playWith clobber [resource]
    pickVillain
    pass
    villainDamageNow `shouldReturn` 3
