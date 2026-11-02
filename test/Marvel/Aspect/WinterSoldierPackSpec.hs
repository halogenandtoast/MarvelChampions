module Marvel.Aspect.WinterSoldierPackSpec (spec) where

import TestImport
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Minion.Cards qualified as Minions
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Stats (Thw(..))
import Marvel.Modifier
import Marvel.Keyword qualified as K
import Marvel.Query
import Marvel.Trait qualified as T

asWinter :: IdentityId -> GameAppT ()
asWinter p = changeForm p >> pass >> clearHand p
abilityOn :: IsRef a => a -> Natural -> Choice -> Bool
abilityOn target n = \case
  UseAbility ab -> abilitySource ab == toSource target && abilityIndex ab == n
  _ -> False

spec :: Spec
spec = describe "Winter Soldier pack" do
  it "Captain America enters tough and may ready a SHIELD character" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      resolve [IdentityMessage p ExhaustedIdentity]
      cap <- putInHand p "54012"
      resources <- traverse (putInHand p) ["01088", "01089"]
      playWith cap resources
      choose "Captain America" (abilityOn (allyIdOf cap) 1)
      choose "ready Winter Soldier" (isTargetChoice p)
      pass
      isExhaustedNow p `shouldReturn` False
      characterMatches (CharacterWithStatus ToughStatus) (AllyCharacter $ allyIdOf cap) `shouldReturn` True

  it "S.H.I.E.L.D. Deputy grants its friendly ally one hit point and the SHIELD trait" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      ally <- putIntoPlay p "01083"
      deputy <- putInHand p "54033"
      playOn deputy (allyIdOf ally) []
      pass
      getModifiers (allyIdOf ally) >>= (`shouldSatisfy` elem (HitPointModifier 1))
      selectAny (AllyWithId (allyIdOf ally) <> AllyWithTrait T.Shield) `shouldReturn` True

  it "S.H.I.E.L.D. Deputy grants its chosen friendly identity one hit point and SHIELD" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      deputy <- putInHand p "54033"
      playOn deputy p []
      pass
      hitPointsOf p `shouldReturn` 11
      identityMatches (IdentityWithTrait T.Shield) p `shouldReturn` True
      resolve [RemoveFromPlay $ toTarget $ upgradeOf deputy]
      hitPointsOf p `shouldReturn` 10
      identityMatches (IdentityWithTrait T.Shield) p `shouldReturn` False

  it "Man on the Wall cannot be played by an identity without Soldier" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      title <- putInHand p "54019"
      _ <- putInHand p "01086"
      offers (isPlay title) `shouldReturn` False

  it "Firepower deals three damage per exhausted Weapon and leaves other upgrades ready" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      rifle <- putIntoPlay p "54011"
      mask <- putIntoPlay p "54010"
      event <- putInHand p "54014"
      resource <- putInHand p "01086"
      playWith event [resource]
      choose "exhaust rifle" (isTargetChoice $ upgradeOf rifle)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 3
      upgradeIsExhausted (upgradeOf rifle) `shouldReturn` True
      upgradeIsExhausted (upgradeOf mask) `shouldReturn` False

  it "Cybernetic Arm raises Firepower's damage for each exhausted Weapon" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      arm <- putIntoPlay p "54002"
      rifle <- putIntoPlay p "54011"
      event <- putInHand p "54014"
      choose "play Firepower" (isPlay event)
      choose "Cybernetic Arm" (abilityOn (upgradeOf arm) 1)
      declareWildAs Physical
      payWith []
      choose "exhaust the rifle" (isTargetChoice $ upgradeOf rifle)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 4

  it "Bambino and S.H.I.E.L.D. Sidearm enter with three ammo counters" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      bambino <- putInHand p "54018"
      resources <- traverse (putInHand p) ["01088", "01086"]
      playOn bambino p resources
      pass
      getCountersOn (upgradeOf bambino) `shouldReturn` 3
      getModifiers (upgradeOf bambino) >>= (`shouldSatisfy` elem (KeywordModifier K.Restricted))
      getModifiers p >>= (`shouldSatisfy` notElem (KeywordModifier K.Restricted))
      sidearm <- putInHand p "54020"
      resource <- putInHand p "01086"
      playOn sidearm p [resource]
      pass
      getCountersOn (upgradeOf sidearm) `shouldReturn` 3

  it "Bambino attaches to the chosen identity and adds three damage to its basic attack" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      weapon <- putInHand p "54018"
      resources <- traverse (putInHand p) ["01088", "01086"]
      playOn weapon p resources
      pass
      choose "basic attack" (isHeroAttack p)
      choose "Bambino" (abilityOn (upgradeOf weapon) 1)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 5
      getCountersOn (upgradeOf weapon) `shouldReturn` 2

  it "S.H.I.E.L.D. Sidearm attaches to the chosen identity and damages an enemy before its attack" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      weapon <- putInHand p "54020"
      resource <- putInHand p "01086"
      playOn weapon p [resource]
      pass
      choose "basic attack" (isHeroAttack p)
      choose "Sidearm" (abilityOn (upgradeOf weapon) 1)
      chooseVillain
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 3
      getCountersOn (upgradeOf weapon) `shouldReturn` 2
      upgradeIsExhausted (upgradeOf weapon) `shouldReturn` True

  it "White Widow has SHIELD and Spy traits with printed two thwart" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      widow <- putIntoPlay (player1 tg) "54032"
      selectAny (AllyWithId (allyIdOf widow) <> AllyWithTrait T.Shield <> AllyWithTrait T.Spy) `shouldReturn` True
      unThw . allyThwart <$> allyIn "54032" `shouldReturn` 2

  it "Deathlok finds a Sidearm from discard and attaches it to himself" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      sidearm <- putInDiscard p "54020"
      deathlok <- putInHand p "54013"
      resources <- traverse (putInHand p) ["01088", "01086"]
      playWith deathlok resources
      choose "Deathlok" (abilityOn (allyIdOf deathlok) 1)
      choose "find Sidearm" (isTargetChoice sidearm)
      pass
      arms <- upgradesWith "54020"
      map upgradeAttachedAlly arms `shouldBe` [Just $ allyIdOf deathlok]

  it "Spoiling for a Fight puts the discarded minion into play and readies the hero" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      choose "attack" (isHeroAttack p)
      chooseVillain
      pass
      _ <- stackEncounterDeck [Cards.fw54031]
      event <- putInHand p "54016"
      playWith event []
      pass
      isExhaustedNow p `shouldReturn` False
      selectAny (MinionIs Cards.fw54031 <> MinionEngagedWith (IdentityWithId p)) `shouldReturn` True

  it "Aggressive Stance searches an Attack event after its hero engages a minion" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      preparation <- putIntoPlay p "54017"
      attacks <- stackDeck p ["54005"]
      _ <- revealNow p Cards.fw54031
      choose "Aggressive Stance" (abilityOn (upgradeOf preparation) 1)
      choose "find Metal Punch" (isTargetChoice $ testHead attacks)
      pass
      handOf p >>= (`shouldSatisfy` elem (testHead attacks))
      upgradeCodes >>= (`shouldSatisfy` notElem "54017")

  it "Super-Soldiers deals six damage and gives both named characters tough" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      cap <- putIntoPlay p "54012"
      event <- putInHand p "54022"
      resources <- traverse (putInHand p) ["01088", "01086"]
      playWith event resources
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 6
      characterHasCondition ToughStatus (IdentityCharacter p) `shouldReturn` True
      characterHasCondition ToughStatus (AllyCharacter $ allyIdOf cap) `shouldReturn` True

  it "Winter, Widow, Soldier, Spy recovers a Preparation before its four damage" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      _ <- putIntoPlay p "54003"
      preparation <- putInDiscard p "54008"
      event <- putInHand p "54023"
      energy <- putInHand p "01088"
      playWith event [energy]
      choose "recover Preparation" (isTargetChoice preparation)
      chooseVillain
      pass
      upgradeCodes >>= (`shouldSatisfy` elem "54008")
      villainDamageNow `shouldReturn` 4

  it "Team-Up attacks cannot be played without their other named character" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      capEvent <- putInHand p "54022"
      widowEvent <- putInHand p "54023"
      _ <- traverse (putInHand p) ["01088", "01089"]
      offers (isPlay capEvent) `shouldReturn` False
      offers (isPlay widowEvent) `shouldReturn` False

  it "White Widow heals the printed cost of a resolved Preparation" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      widow <- putIntoPlay p "54032"
      resolve [AllyMessage (allyIdOf widow) $ AllySetDamage 2]
      preparation <- putIntoPlay p "54008"
      mid <- addMinion p Minions.weaponsRunner
      choose "attack" (isHeroAttack p)
      choose "minion" (isTargetChoice mid)
      choose "Silent Infiltration" (abilityOn (upgradeOf preparation) 1)
      chooseVillain
      choose "White Widow" (abilityOn (allyIdOf widow) 1)
      pass
      allyDamage <$> allyIn "54032" `shouldReturn` 0
