module Marvel.Hero.WarMachineSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Cards qualified as Minions
import Marvel.Phase (Phase (..))
import Marvel.Trait (HasTraits (getTraits), Trait (Aerial))
import TestImport

warMachine :: IO TestGame
warMachine = testGame ["war-machine"]

ownAbility :: (IsRef a) => a -> Natural -> Choice -> Bool
ownAbility target index = \case
  UseAbility ab -> abilitySource ab == toRef target && abilityIndex ab == index
  _ -> False

ammoOf :: IdentityId -> GameAppT Natural
ammoOf p = playerIdentityCounters <$> playerAttrs p

asHero :: IdentityId -> GameAppT ()
asHero p = do
  changeForm p
  choose "Locked and Loaded" (ownAbility p 1)
  pass
  clearHand p

setAmmo :: IdentityId -> Natural -> GameAppT ()
setAmmo p n = withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
  PlayerIdentity a {playerIdentityCounters = n}

asAlterEgo :: IdentityId -> GameAppT ()
asAlterEgo p = resolve [IdentityMessage p (ChangedToForm B)]

bunkerUses :: PlayerCard -> GameAppT Natural
bunkerUses card = getsGame (maybe 0 (supportUses . toAttrs) . HashMap.lookup (supportIdOf card) . gameSupports)

bunkerExhausted :: PlayerCard -> GameAppT Bool
bunkerExhausted card = getsGame (maybe False (supportExhausted . toAttrs) . HashMap.lookup (supportIdOf card) . gameSupports)

payCodes :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
payCodes p card codes = traverse (putInHand p) codes >>= playWith card

finishResources :: GameAppT ()
finishResources = whenM (offers isFinishPayment) (choose "finish paying" isFinishPayment)

spec :: Spec
spec = describe "War Machine hero" do
  describe "War Machine (23001a)" do
    it "starts without ammo and may decline Locked and Loaded" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        ammoOf p `shouldReturn` 0
        changeForm p
        pass
        ammoOf p `shouldReturn` 0
        handSizeOf p `shouldReturn` 5

    it "places five counters in addition to counters already on the identity" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        setAmmo p 2
        changeForm p
        choose "load five" (ownAbility p 1)
        pass
        ammoOf p `shouldReturn` 7

    it "does not answer another identity changing form" do
      tg <- testGame ["war-machine", "captain-marvel"]
      let p = player1 tg
      inGame tg do
        runNow [IdentityMessage (player2 tg) (ChangedToForm A)]
        offers (ownAbility p 1) `shouldReturn` False
        pass
        ammoOf p `shouldReturn` 0

  describe "James Rhodes (23001b)" do
    it "forcibly clears all ammo when changing to alter ego" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        ammoOf p `shouldReturn` 5
        asAlterEgo p
        ammoOf p `shouldReturn` 0
        handSizeOf p `shouldReturn` 6

    it "shuffles one signature card, excludes basic/aspect cards, and is limited once per phase" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        [signature, otherSignature, basic] <- stackDiscard p ["23008", "23009", "01088"]
        choose "use James Rhodes" (ownAbility p 2)
        offers (isTargetChoice basic) `shouldReturn` False
        choose "shuffle Repulsor Beam" (isTargetChoice signature)
        discardOf p >>= (`shouldSatisfy` (\cs -> signature `notElem` cs && otherSignature `elem` cs))
        deck <- unDeck . playerIdentityDeck <$> playerAttrs p
        deck `shouldSatisfy` elem signature
        offers (ownAbility p 2) `shouldReturn` False
        runNow [BeginPhase PlayerPhase]
        offers (ownAbility p 2) `shouldReturn` True

    it "has no shuffle action without a signature card in its own discard pile" do
      tg <- testGame ["war-machine", "captain-marvel"]
      let p = player1 tg
      inGame tg do
        withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) -> PlayerIdentity a {playerIdentityDiscard = Discard []}
        _ <- stackDiscard p ["01088", "01071"]
        _ <- stackDiscard (player2 tg) ["23008"]
        offers (ownAbility p 2) `shouldReturn` False

    it "does not suppress the hero's response after using the alter ego's phase-limited action" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        [signature] <- stackDiscard p ["23008"]
        choose "shuffle a signature" (ownAbility p 2)
        choose "shuffle it" (isTargetChoice signature)
        changeForm p
        choose "load as hero" (ownAbility p 1)
        pass
        ammoOf p `shouldReturn` 5

  describe "Iron Man (23002)" do
    it "searches both deck and discard for a Tech upgrade and adds exactly the chosen one" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        clearHand p
        [inDeck] <- stackDeck p ["23005"]
        [inDiscard] <- stackDiscard p ["23004"]
        iron <- putInHand p "23002"
        payCodes p iron ["01088", "01089"]
        choose "search with Iron Man" (isAbilityOfCard iron)
        offers (isTargetChoice inDeck) `shouldReturn` True
        offers (isTargetChoice inDiscard) `shouldReturn` True
        choose "take the discarded chassis" (isTargetChoice inDiscard)
        pass
        handOf p >>= (`shouldSatisfy` elem inDiscard)
        discardOf p >>= (`shouldSatisfy` notElem inDiscard)
        deck <- unDeck . playerIdentityDeck <$> playerAttrs p
        deck `shouldSatisfy` elem inDeck
        ally <- allyIn "23002"
        ally.allyAttackConsequentialDamage `shouldBe` 1
        ally.allyThwartConsequentialDamage `shouldBe` 1

    it "does not find a non-Tech upgrade or a Tech card that is not an upgrade" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        clearHand p
        [nonTech, support] <- stackDiscard p ["01057", "23003"]
        iron <- putInHand p "23002"
        payCodes p iron ["01088", "01089"]
        choose "search" (isAbilityOfCard iron)
        offers (isTargetChoice nonTech) `shouldReturn` False
        offers (isTargetChoice support) `shouldReturn` False

    it "may decline the search response" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        clearHand p
        [tech] <- stackDiscard p ["23005"]
        iron <- putInHand p "23002"
        payCodes p iron ["01088", "01089"]
        pass
        handOf p >>= (`shouldSatisfy` notElem tech)
        discardOf p >>= (`shouldSatisfy` elem tech)

  describe "Munitions Bunker (23003)" do
    it "exhausts to store two ammo in alter ego and can accumulate more after readying" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        clearHand p
        bunker <- putInHand p "23003"
        payCodes p bunker ["01088"]
        choose "store ammo" (ownAbility (supportIdOf bunker) 1)
        bunkerUses bunker `shouldReturn` 2
        bunkerExhausted bunker `shouldReturn` True
        offers (ownAbility (supportIdOf bunker) 1) `shouldReturn` False
        resolve [SupportMessage (supportIdOf bunker) ReadiedSupport]
        choose "store more ammo" (ownAbility (supportIdOf bunker) 1)
        bunkerUses bunker `shouldReturn` 4
        ammoOf p `shouldReturn` 0

    it "moves every stored ammo to the hero, empties, exhausts and remains in play" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        bunker <- putIntoPlay p "23003"
        choose "store two" (ownAbility (supportIdOf bunker) 1)
        resolve [SupportMessage (supportIdOf bunker) ReadiedSupport]
        choose "store two more" (ownAbility (supportIdOf bunker) 1)
        resolve [SupportMessage (supportIdOf bunker) ReadiedSupport]
        asHero p
        choose "transfer all four" (ownAbility (supportIdOf bunker) 2)
        ammoOf p `shouldReturn` 9
        bunkerUses bunker `shouldReturn` 0
        bunkerExhausted bunker `shouldReturn` True
        resolve [SupportMessage (supportIdOf bunker) ReadiedSupport]
        offers (ownAbility (supportIdOf bunker) 2) `shouldReturn` False
        getsGame (HashMap.member (supportIdOf bunker) . gameSupports) `shouldReturn` True

    it "offers storage only in alter ego and transfer only in hero form" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        bunker <- putIntoPlay p "23003"
        offers (ownAbility (supportIdOf bunker) 2) `shouldReturn` False
        asHero p
        offers (ownAbility (supportIdOf bunker) 1) `shouldReturn` False

  describe "Upgraded Chassis (23004)" do
    it "gives Aerial only to the hero, and exhausts after changing form to give tough" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        clearHand p
        chassis <- putInHand p "23004"
        payCodes p chassis ["01088", "01089"]
        (getTraits =<< getPlayer p) >>= (`shouldSatisfy` notElem Aerial)
        changeForm p
        choose "give War Machine tough" (isAbilityOfCard chassis)
        pass
        playerIdentityTough <$> playerAttrs p `shouldReturn` True
        upgradeIsExhausted (upgradeIdOf chassis) `shouldReturn` True
        (getTraits =<< getPlayer p) >>= (`shouldSatisfy` elem Aerial)
        asAlterEgo p
        (getTraits =<< getPlayer p) >>= (`shouldSatisfy` notElem Aerial)

    it "does not trigger when already tough or when the chassis is exhausted" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        chassis <- putIntoPlay p "23004"
        resolve [IdentityMessage p IdentityBecomeTough]
        changeForm p
        offers (isAbilityOfCard chassis) `shouldReturn` False
        pass
        asAlterEgo p
        withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) -> PlayerIdentity a {playerIdentityTough = False}
        resolve [UpgradeMessage (upgradeIdOf chassis) ExhaustedUpgrade]
        runNow [IdentityMessage p (ChangedToForm A)]
        offers (isAbilityOfCard chassis) `shouldReturn` False

    it "may decline the tough response without exhausting the chassis" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        chassis <- putIntoPlay p "23004"
        changeForm p
        pass
        upgradeIsExhausted (upgradeIdOf chassis) `shouldReturn` False
        playerIdentityTough <$> playerAttrs p `shouldReturn` False

  describe "Gauntlet Gun (23005)" do
    it "generates a wild and one ammo for a signature event, exhausting the gun" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        gun <- putInHand p "23005"
        payCodes p gun ["01088"]
        clearHand p
        setAmmo p 1
        beam <- putInHand p "23008"
        choose "play Repulsor Beam" (isPlay beam)
        choose "pay with Gauntlet Gun" (isAbilityOfCard gun)
        ammoOf p `shouldReturn` 2
        finishResources
        chooseVillain
        pass
        ammoOf p `shouldReturn` 1
        upgradeIsExhausted (upgradeIdOf gun) `shouldReturn` True

    it "cannot be used as an action or for a basic/aspect card" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        gun <- putIntoPlay p "23005"
        offers (isAbilityOfCard gun) `shouldReturn` False
        basic <- putInHand p "01092"
        _ <- putInHand p "01088"
        _ <- putInHand p "01089"
        choose "play Helicarrier" (isPlay basic)
        offers (isAbilityOfCard gun) `shouldReturn` False

    it "can supply the missing ammo while paying a signature event that starts at zero ammo" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        gun <- putIntoPlay p "23005"
        clearHand p
        setAmmo p 0
        beam <- putInHand p "23008"
        offers (isPlay beam) `shouldReturn` True
        choose "play at zero ammo" (isPlay beam)
        choose "generate resource and needed ammo" (isAbilityOfCard gun)
        finishResources
        chooseVillain
        pass
        ammoOf p `shouldReturn` 0
        villainDamageNow `shouldReturn` 4

    it "combines two guns to supply two missing ammo for Full Auto" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        gun1 <- putIntoPlay p "23005"
        gun2 <- putIntoPlay p "23005"
        clearHand p
        setAmmo p 2
        fullAuto <- putInHand p "23011"
        choose "play Full Auto" (isPlay fullAuto)
        chooseVillain
        choose "use first gun" (isAbilityOfCard gun1)
        offers isFinishPayment `shouldReturn` False
        choose "use second gun" (isAbilityOfCard gun2)
        finishResources
        pass
        ammoOf p `shouldReturn` 0
        villainDamageNow `shouldReturn` 8

    it "may overpay a one-resource event with both guns to gain net ammo" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        gun1 <- putIntoPlay p "23005"
        gun2 <- putIntoPlay p "23005"
        clearHand p
        setAmmo p 1
        beam <- putInHand p "23008"
        choose "play Beam" (isPlay beam)
        choose "use first gun" (isAbilityOfCard gun1)
        choose "overpay with second gun" (isAbilityOfCard gun2)
        finishResources
        chooseVillain
        pass
        ammoOf p `shouldReturn` 2

    it "does not allow finishing before the ammo cost is payable" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        gun <- putIntoPlay p "23005"
        clearHand p
        setAmmo p 0
        beam <- putInHand p "23008"
        energy <- putInHand p "01088"
        choose "play Beam" (isPlay beam)
        choose "pay ordinary resources first" (isPayWith energy)
        offers isFinishPayment `shouldReturn` False
        offers (isAbilityOfCard gun) `shouldReturn` True

    it "cannot use an exhausted gun to make a zero-ammo event affordable" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        gun <- putIntoPlay p "23005"
        clearHand p
        setAmmo p 0
        resolve [UpgradeMessage (upgradeIdOf gun) ExhaustedUpgrade]
        beam <- putInHand p "23008"
        _ <- putInHand p "01088"
        offers (isPlay beam) `shouldReturn` False

    it "can generate required ammo even when the event's resource cost was reduced to zero" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        gun <- putIntoPlay p "23005"
        carrier <- putIntoPlay p "01092"
        clearHand p
        setAmmo p 0
        choose "reduce the next card's cost" (isAbilityOfCard carrier)
        choose "choose War Machine" (isTargetChoice p)
        beam <- putInHand p "23008"
        choose "play the free event" (isPlay beam)
        choose "generate its ammo" (isAbilityOfCard gun)
        finishResources
        chooseVillain
        pass
        ammoOf p `shouldReturn` 0
        villainDamageNow `shouldReturn` 4

  describe "Missile Launcher (23006)" do
    it "spends one ammo, exhausts, and makes a two-damage ranged attack" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        missile <- putInHand p "23006"
        payCodes p missile ["01088"]
        enemy <- addMinion p Minions.whiplash
        before <- damageOf p
        choose "fire Missile Launcher" (isAbilityOfCard missile)
        choose "target Whiplash" (isTargetChoice enemy)
        pass
        minionDamageOf enemy `shouldReturn` Just 2
        damageOf p `shouldReturn` before
        ammoOf p `shouldReturn` 4
        upgradeIsExhausted (upgradeIdOf missile) `shouldReturn` True

    it "cannot fire in alter ego or without ammo, even with a ready Gauntlet Gun" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        missile <- putIntoPlay p "23006"
        _ <- putIntoPlay p "23005"
        offers (isAbilityOfCard missile) `shouldReturn` False
        asHero p
        setAmmo p 0
        offers (isAbilityOfCard missile) `shouldReturn` False

    it "pays the attack costs and removes stun instead of damaging an enemy" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        missile <- putIntoPlay p "23006"
        resolve [IdentityMessage p IdentityStunned]
        choose "attempt the attack" (isAbilityOfCard missile)
        pass
        isStunned p `shouldReturn` False
        ammoOf p `shouldReturn` 4
        upgradeIsExhausted (upgradeIdOf missile) `shouldReturn` True
        villainDamageNow `shouldReturn` 0

  describe "Shoulder Cannon (23007)" do
    it "can fire for one damage at zero ammo and remains exhausted" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        cannon <- putInHand p "23007"
        payCodes p cannon ["01088"]
        setAmmo p 0
        choose "fire" (isAbilityOfCard cannon)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 1
        upgradeIsExhausted (upgradeIdOf cannon) `shouldReturn` True
        offers (isLabel "$label.readyShoulderCannon") `shouldReturn` False

    it "can spend one ammo to ready after each attack and fire again" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        cannon <- putIntoPlay p "23007"
        setAmmo p 2
        choose "fire first time" (isAbilityOfCard cannon)
        chooseVillain
        choose "spend ammo to ready" (isLabel "$label.readyShoulderCannon")
        pass
        ammoOf p `shouldReturn` 1
        upgradeIsExhausted (upgradeIdOf cannon) `shouldReturn` False
        choose "fire again" (isAbilityOfCard cannon)
        chooseVillain
        choose "spend remaining ammo" (isLabel "$label.readyShoulderCannon")
        pass
        ammoOf p `shouldReturn` 0
        villainDamageNow `shouldReturn` 2
        upgradeIsExhausted (upgradeIdOf cannon) `shouldReturn` False

    it "finishes its optional ready before after-attack responses are offered" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        cannon <- putIntoPlay p "23007"
        jarnbjorn <- putIntoPlay p "06019"
        _ <- putInHand p "01090"
        choose "fire" (isAbilityOfCard cannon)
        chooseVillain
        offers (isAbilityOfCard jarnbjorn) `shouldReturn` False
        choose "finish Cannon's own ready effect" (isLabel "$label.readyShoulderCannon")
        ammoOf p `shouldReturn` 4
        upgradeIsExhausted (upgradeIdOf cannon) `shouldReturn` False
        offers (isAbilityOfCard jarnbjorn) `shouldReturn` True
        pass
        villainDamageNow `shouldReturn` 1

    it "can decline readying and takes retaliate because its attack is not ranged" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        cannon <- putIntoPlay p "23007"
        enemy <- addMinion p Minions.whiplash
        choose "fire at Whiplash" (isAbilityOfCard cannon)
        choose "target him" (isTargetChoice enemy)
        choose "do not spend ammo" (isLabel "$label.done")
        pass
        ammoOf p `shouldReturn` 5
        damageOf p `shouldReturn` 1
        upgradeIsExhausted (upgradeIdOf cannon) `shouldReturn` True

    it "is unavailable in alter ego and does not offer readying when stun replaces its attack" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        cannon <- putIntoPlay p "23007"
        offers (isAbilityOfCard cannon) `shouldReturn` False
        asHero p
        resolve [IdentityMessage p IdentityStunned]
        choose "attempt an attack" (isAbilityOfCard cannon)
        offers (isLabel "$label.readyShoulderCannon") `shouldReturn` False
        ammoOf p `shouldReturn` 5
        villainDamageNow `shouldReturn` 0
