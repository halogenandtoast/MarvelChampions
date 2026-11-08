module Marvel.Hero.MariaHillSpec (spec) where
import Marvel.Game.Source
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import Marvel.Matchers
import Marvel.Query
import Marvel.Trait qualified as T
import TestImport

onCard :: PlayerCard -> Natural -> Choice -> Bool
onCard card n = \case UseAbility ab -> isAbilityOfCard card (UseAbility ab) && abilityIndex ab == n; _ -> False

spec :: Spec
spec = describe "Maria Hill" do
  it "starts with nine hit points and recovers three" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      hitPointsOf p `shouldReturn` 9
      handSizeOf p `shouldReturn` 6
      setDamage p 5
      choose "recover" \case UseAbility a -> Recover `elem` abilityChoices a; _ -> False
      damageOf p `shouldReturn` 2

  it "gives her allies the SHIELD trait only while Maria is in hero form" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      ally <- putIntoPlay p "01050"
      selectAny (AllyWithId (allyIdOf ally) <> AllyWithTrait T.Shield) `shouldReturn` False
      changeForm p
      pass
      selectAny (AllyWithId (allyIdOf ally) <> AllyWithTrait T.Shield) `shouldReturn` True

  it "the Iliad spends one mission counter and exhausts to deal five damage" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      iliad <- putIntoPlay p "50009"
      getCountersOn (supportIdOf iliad) `shouldReturn` 3
      choose "The Iliad" (onCard iliad 1)
      choose "damage" (isLabel "$label.dealDamage")
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 5
      getCountersOn (supportIdOf iliad) `shouldReturn` 2
      offers (onCard iliad 1) `shouldReturn` False

  it "SHIELD Director adds a counter to a chosen SHIELD support" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      iliad <- putIntoPlay p "50009"
      director <- putIntoPlay p "50011"
      choose "Director" (onCard director 1)
      choose "Iliad" (isTargetChoice $ supportIdOf iliad)
      pass
      getCountersOn (supportIdOf iliad) `shouldReturn` 4

  it "her alter-ego search does not consume Reassignment's once-per-round limit" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      clearHand p
      cards <- stackDeck p ["50008", "01090"]
      choose "alter-ego search" (\case UseAbility a -> abilityIndex a == 1 && abilitySource a == toSource p; _ -> False)
      choose "find support" (isTargetChoice $ testHead cards)
      pass
      changeForm p
      pass
      staff <- putIntoPlay p "50008"
      iliad <- putIntoPlay p "50009"
      choose "Reassignment" (\case UseAbility a -> abilityIndex a == 1 && abilitySource a == toSource p; _ -> False)
      choose "from staff" (isTargetChoice $ supportIdOf staff)
      choose "to Iliad" (isTargetChoice $ supportIdOf iliad)
      pass
      getCountersOn (supportIdOf staff) `shouldReturn` 2
      getCountersOn (supportIdOf iliad) `shouldReturn` 4
      offers (\case UseAbility a -> abilityIndex a == 1 && abilitySource a == toSource p; _ -> False) `shouldReturn` False

  it "moves one mission counter between supports once each round" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      staff <- putIntoPlay p "50008"
      iliad <- putIntoPlay p "50009"
      choose "Maria's coordination" (\case UseAbility a -> abilityIndex a == 1 && abilitySource a == toSource p; _ -> False)
      choose "from staff" (isTargetChoice $ supportIdOf staff)
      offers (isTargetChoice $ supportIdOf staff) `shouldReturn` False
      choose "to Iliad" (isTargetChoice $ supportIdOf iliad)
      pass
      getCountersOn (supportIdOf staff) `shouldReturn` 2
      getCountersOn (supportIdOf iliad) `shouldReturn` 4
      offers (\case UseAbility a -> abilityIndex a == 1 && abilitySource a == toSource p; _ -> False) `shouldReturn` False

  it "Iliad can remove four threat instead of dealing damage" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      iliad <- putIntoPlay p "50009"
      placeMainThreat 5
      choose "Iliad" (onCard iliad 1)
      choose "remove threat" (isLabel "$label.removeThreat")
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` 1
      getCountersOn (supportIdOf iliad) `shouldReturn` 2

  it "Iliad can heal a chosen identity for three" do
    tg <- testGame ["maria-hill", "spider-man"]
    inGame tg do
      let p = player1 tg
      iliad <- putIntoPlay p "50009"
      setDamage (player2 tg) 5
      choose "Iliad" (onCard iliad 1)
      choose "heal" (isLabel "$label.heal")
      choose "Spider-Man" (isTargetChoice $ player2 tg)
      pass
      damageOf (player2 tg) `shouldReturn` 2

  it "Life Model Decoy prevents the attack's damage and is discarded" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      decoy <- putIntoPlay p "50010"
      mid <- addMinion p Minions.hydraSoldier
      L.runQueueT (L.capture $ L.enemyAttacks mid p) >>= start
      choose "Life Model Decoy" (onCard decoy 1)
      pass
      damageOf p `shouldReturn` 0
      map toCardCode <$> discardOf p `shouldReturn` ["50010"]
