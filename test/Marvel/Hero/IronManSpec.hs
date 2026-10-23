module Marvel.Hero.IronManSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Cards qualified as Cards
import Marvel.Obligation.Cards qualified as Cards
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types (getSideSchemeThreat)
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Ally.Types (getAllyDamage)
import Marvel.Trait (HasTraits (..), Trait (Aerial))
import TestImport

spec :: Spec
spec = describe "Iron Man" do
  it "Iron Man gets +1 hand size for each Tech upgrade, to at most 7" do
    tg <- testGame ["iron-man"]
    let p = player1 tg
    inGame tg do
      handSizeOf p `shouldReturn` 6
      changeForm p
      handSizeOf p `shouldReturn` 1
      _ <- putIntoPlay p "01036"
      _ <- putIntoPlay p "01037"
      handSizeOf p `shouldReturn` 3
      for_ ["01035", "01038", "01038", "01039", "01039"] (putIntoPlay p)
      handSizeOf p `shouldReturn` 7

  it "Tony Stark's Futurist adds 1 of the top 3 cards to his hand, discards the rest, once a round" do
    tg <- testGame ["iron-man"]
    let p = player1 tg
    inGame tg do
      [a, b, c] <- stackDeck p ["01031", "01032", "01036"]
      before <- length <$> handOf p
      choose "use Futurist" (isLabeledAbility "Futurist")
      choose "take Supersonic Punch" (isTargetChoice b)
      hand <- handOf p
      length hand `shouldBe` before + 1
      (b `elem` hand) `shouldBe` True
      discard <- discardOf p
      (a `elem` discard && c `elem` discard) `shouldBe` True
      offers (isLabeledAbility "Futurist") `shouldReturn` False

  it "Futurist is an alter-ego ability" do
    tg <- testGame ["iron-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      offers (isLabeledAbility "Futurist") `shouldReturn` False

  it "War Machine takes 2 damage and exhausts to deal 1 damage to each enemy" do
    tg <- testGame ["iron-man"]
    let p = player1 tg
    inGame tg do
      mercenary <- encounterCard Cards.hydraMercenary
      resolve [RevealEncounterCard p mercenary]
      rhodey <- putIntoPlay p "01030"
      choose "use War Machine" (isAbilityOf (allyIdOf rhodey))
      villainDamageNow `shouldReturn` 1
      minionDamages `shouldReturn` [1]
      allies <- getsGame (HashMap.elems . gameAllies)
      map getAllyDamage allies `shouldBe` [2]
      offers (isAbilityOf (allyIdOf rhodey)) `shouldReturn` False

  it "Repulsor Blast deals 1 damage and 2 more for each energy resource among the top 5 cards" do
    tg <- testGame ["iron-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      blast <- putInHand p "01031"
      strength <- putInHand p "01090"
      -- Supersonic Punch, Arc Reactor and Energy (2): 4 energy; Vibranium's wilds are not energy
      _ <- stackDeck p ["01032", "01035", "01088", "01044", "01036"]
      offers (isPlay blast) `shouldReturn` False
      changeForm p
      playWith blast [strength]
      villain <- getVillain
      choose "blast Rhino" (isTargetChoice (EnemyVillainId (toId villain)))
      villainDamageNow `shouldReturn` 9
      length <$> discardOf p `shouldReturn` 7

  it "Supersonic Punch deals 4 damage, 8 with the Aerial trait" do
    tg <- testGame ["iron-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      punch <- putInHand p "01032"
      punch2 <- putInHand p "01032"
      pay <- for ["01088", "01088", "01089"] (putInHand p)
      changeForm p
      villain <- getVillain
      playWith punch [pay !! 0]
      choose "punch Rhino" (isTargetChoice (toId villain))
      villainDamageNow `shouldReturn` 4
      boots <- putIntoPlay p "01039"
      choose "use Rocket Boots" (isAbilityOf (upgradeIdOf boots))
      choose "pay with Genius" (isPayWith (pay !! 2))
      whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment
      playWith punch2 [pay !! 1]
      choose "punch Rhino" (isTargetChoice (toId villain))
      villainDamageNow `shouldReturn` 12

  it "Pepper Potts generates the resources of the top card of the discard pile" do
    tg <- testGame ["iron-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      _ <- putIntoPlay p "01033"
      -- Energy, with two energy resources, on top of the discard pile
      _ <- stackDiscard p ["01088"]
      punch <- putInHand p "01032"
      changeForm p
      choose "play Supersonic Punch" (isPlay punch)
      choose "pay with Pepper Potts" \case
        UseAbility ab -> case abilitySource ab of
          SupportRef _ -> True
          _ -> False
        _ -> False
      whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment
      villain <- getVillain
      choose "punch Rhino" (isTargetChoice (toId villain))
      villainDamageNow `shouldReturn` 4

  it "Stark Tower returns the topmost Tech upgrade in a discard pile to hand, in alter-ego form" do
    tg <- testGame ["iron-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      tower <- putIntoPlay p "01034"
      [_, helmet, _] <- stackDiscard p ["01032", "01037", "01036"]
      choose "use Stark Tower" (isAbilityOf (supportIdOf tower))
      choose "the player" (isTargetChoice p)
      handOf p `shouldReturn` [helmet]
      changeForm p
      offers (isAbilityOf (supportIdOf tower)) `shouldReturn` False

  it "Arc Reactor readies Iron Man" do
    tg <- testGame ["iron-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      reactor <- putIntoPlay p "01035"
      offers (isAbilityOf (upgradeIdOf reactor)) `shouldReturn` False
      resolve [IdentityMessage p ExhaustedIdentity]
      choose "use Arc Reactor" (isAbilityOf (upgradeIdOf reactor))
      isExhaustedPlayer p `shouldReturn` False
      upgradeIsExhausted (upgradeIdOf reactor) `shouldReturn` True

  it "Mark V Armor gives +6 hit points and Rocket Boots +1" do
    tg <- testGame ["iron-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      hitPointsOf p `shouldReturn` 9
      _ <- putIntoPlay p "01036"
      hitPointsOf p `shouldReturn` 15
      _ <- putIntoPlay p "01039"
      hitPointsOf p `shouldReturn` 16

  it "Mark V Helmet removes 1 threat from a scheme, from each scheme with Aerial" do
    tg <- testGame ["iron-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      ms <- mainSchemeId
      resolve [MainSchemeMessage ms (MainSchemePlaceThreat 5)]
      before <- mainSchemeThreatNow
      helmet <- putIntoPlay p "01037"
      choose "use Mark V Helmet" (isAbilityOf (upgradeIdOf helmet))
      whenM (offers \case TargetLabel {} -> True; _ -> False) $ choose "the main scheme" (isTargetChoice ms)
      mainSchemeThreatNow `shouldReturn` (before - 1)
      overload <- encounterCard Cards.imminentOverload
      resolve [RevealEncounterCard p overload]
      sideBefore <- getsGame (map getSideSchemeThreat . HashMap.elems . gameSideSchemes)
      resolve [UpgradeMessage (upgradeIdOf helmet) ReadiedUpgrade]
      boots <- putIntoPlay p "01039"
      mental <- putInHand p "01089"
      choose "use Rocket Boots" (isAbilityOf (upgradeIdOf boots))
      choose "pay with mental" (isPayWith mental)
      whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment
      choose "use Mark V Helmet" (isAbilityOf (upgradeIdOf helmet))
      mainSchemeThreatNow `shouldReturn` (before - 2)
      getsGame (map getSideSchemeThreat . HashMap.elems . gameSideSchemes) `shouldReturn` map (subtract 1) sideBefore

  it "Powered Gauntlets deal 1 damage, 2 with Aerial" do
    tg <- testGame ["iron-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      villain <- getVillain
      gauntlets <- putIntoPlay p "01038"
      choose "use Powered Gauntlets" (isAbilityOf (upgradeIdOf gauntlets))
      choose "hit Rhino" (isTargetChoice (toId villain))
      villainDamageNow `shouldReturn` 1
      resolve [UpgradeMessage (upgradeIdOf gauntlets) ReadiedUpgrade]
      boots <- putIntoPlay p "01039"
      mental <- putInHand p "01089"
      choose "use Rocket Boots" (isAbilityOf (upgradeIdOf boots))
      choose "pay with mental" (isPayWith mental)
      whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment
      choose "use Powered Gauntlets" (isAbilityOf (upgradeIdOf gauntlets))
      choose "hit Rhino" (isTargetChoice (toId villain))
      villainDamageNow `shouldReturn` 3

  it "Powered Gauntlets' attack only removes the stun from a stunned Iron Man" do
    tg <- testGame ["iron-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      gauntlets <- putIntoPlay p "01038"
      resolve [IdentityMessage p IdentityStunned]
      choose "use Powered Gauntlets" (isAbilityOf (upgradeIdOf gauntlets))
      villainDamageNow `shouldReturn` 0
      (.playerIdentityStunned) . toAttrs <$> getPlayer p `shouldReturn` False

  it "Rocket Boots' Aerial lasts until the end of the phase" do
    tg <- testGame ["iron-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      boots <- putIntoPlay p "01039"
      clearHand p
      offers (isAbilityOf (upgradeIdOf boots)) `shouldReturn` False
      mental <- putInHand p "01089"
      choose "use Rocket Boots" (isAbilityOf (upgradeIdOf boots))
      choose "pay with mental" (isPayWith mental)
      whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment
      aerial p `shouldReturn` True
      endTurn
      pass
      aerial p `shouldReturn` False

  it "Business Problems: exhaust Tony Stark to remove it from the game" do
    tg <- testGame ["iron-man"]
    let p = player1 tg
    inGame tg do
      problems <- encounterCard Cards.businessProblems
      resolve [RevealEncounterCard p problems]
      isExhaustedPlayer p `shouldReturn` True
      obligationsInPlay `shouldReturn` 0
      encounterDiscardCodes `shouldReturn` []

  it "Business Problems: otherwise exhaust each upgrade and discard it" do
    tg <- testGame ["iron-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      helmet <- putIntoPlay p "01037"
      armor <- putIntoPlay p "01036"
      problems <- encounterCard Cards.businessProblems
      start [RevealEncounterCard p problems]
      choose "stay Iron Man" (isLabel "$label.doNotFlipToAlterEgo")
      isExhaustedPlayer p `shouldReturn` False
      upgradeIsExhausted (upgradeIdOf helmet) `shouldReturn` True
      upgradeIsExhausted (upgradeIdOf armor) `shouldReturn` True
      obligationsInPlay `shouldReturn` 0
      encounterDiscardCodes `shouldReturn` ["01170"]

  it "Business Problems is given to the Tony Stark player whoever reveals it" do
    tg <- testGame ["spider-man", "iron-man"]
    let (spidey, tony) = case tg.seats of
          [a, b] -> (a, b)
          _ -> error "two players"
    inGame tg do
      problems <- encounterCard Cards.businessProblems
      resolve [RevealEncounterCard spidey problems]
      isExhaustedPlayer tony `shouldReturn` True
      isExhaustedPlayer spidey `shouldReturn` False

  it "Imminent Overload enters with 3 threat and 1 more per hero" do
    tg <- testGame ["iron-man"]
    let p = player1 tg
    inGame tg do
      overload <- encounterCard Cards.imminentOverload
      resolve [RevealEncounterCard p overload]
      getsGame (map getSideSchemeThreat . HashMap.elems . gameSideSchemes) `shouldReturn` [4]

  it "Whiplash retaliates 1 when Iron Man attacks him" do
    tg <- testGame ["iron-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      whiplash <- encounterCard Cards.whiplash
      resolve [RevealEncounterCard p whiplash]
      gauntlets <- putIntoPlay p "01038"
      choose "use Powered Gauntlets" (isAbilityOf (upgradeIdOf gauntlets))
      choose "hit Whiplash" \case
        TargetLabel (MinionRef _) _ -> True
        _ -> False
      minionDamages `shouldReturn` [1]
      damageOf p `shouldReturn` 1

  it "Electric Whip Attack deals 1 damage for each upgrade, or discards one" do
    tg <- testGame ["iron-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      _ <- putIntoPlay p "01037"
      armor <- putIntoPlay p "01036"
      whip <- encounterCard Cards.electricWhipAttack
      resolve [RevealEncounterCard p whip]
      damageOf p `shouldReturn` 2
      whip2 <- encounterCard Cards.electricWhipAttack
      start [RevealEncounterCard p whip2]
      choose "discard an upgrade" \case
        Label l _ -> l /= "$label.deal1DamageToYourHeroForEachUpgradeYouControl"
        _ -> False
      choose "discard the armor" (isTargetChoice (upgradeIdOf armor))
      length <$> upgradesOf p `shouldReturn` 1
      damageOf p `shouldReturn` 2

  it "Electric Whip Attack's boost discards an upgrade when the villain's attack is undefended" do
    tg <- testGame ["iron-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      helmet <- putIntoPlay p "01037"
      villain <- getVillain
      whip <- encounterCard Cards.electricWhipAttack
      start [VillainMessage (toId villain) (VillainDealtBoost whip), VillainMessage (toId villain) (VillainAttacks p)]
      pass
      choose "discard the helmet" (isTargetChoice (upgradeIdOf helmet))
      upgradesOf p `shouldReturn` []

  it "Electromagnetic Backlash: each player discards 5 and takes 1 damage per energy" do
    tg <- testGame ["iron-man"]
    let p = player1 tg
    inGame tg do
      _ <- stackDeck p ["01032", "01035", "01088", "01044", "01036"]
      backlash <- encounterCard Cards.electromagneticBacklash
      resolve [RevealEncounterCard p backlash]
      damageOf p `shouldReturn` 4
      length <$> discardOf p `shouldReturn` 5
 where
  aerial p = do
    player <- getPlayer p
    traits <- getTraits player
    pure (Aerial `elem` traits)
