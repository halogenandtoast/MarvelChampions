module Marvel.Hero.WinterSoldierSpec (spec) where

import TestImport
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Query
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Keyword
import Marvel.Trait qualified as T

asWinter :: IdentityId -> GameAppT ()
asWinter p = changeForm p >> pass >> clearHand p

abilityOn :: IsRef a => a -> Natural -> Choice -> Bool
abilityOn target n = \case
  UseAbility ab -> abilitySource ab == toSource target && abilityIndex ab == n
  _ -> False

spec :: Spec
spec = describe "Winter Soldier" do
  it "starts as Bucky with eleven hit points and a six-card alter-ego hand" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      hitPointsOf (player1 tg) `shouldReturn` 11
      handSizeOf (player1 tg) `shouldReturn` 6
      asWinter (player1 tg)
      handSizeOf (player1 tg) `shouldReturn` 5

  it "Metal Punch deals seven damage without the Cybernetic Arm" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      punch <- putInHand p "54005"
      resources <- traverse (putInHand p) ["01088", "01086"]
      playWith punch resources
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 7

  it "Electrical Discharge stuns only when an energy resource was actually paid" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      discharge <- putInHand p "54006"
      energy <- putInHand p "01088"
      playWith discharge [energy]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 4
      v <- villainIdNow
      selectAny (EnemyWithId (EnemyVillainId v) <> StunnedEnemy) `shouldReturn` True

  it "Electrical Discharge does not stun for mental payment" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      discharge <- putInHand p "54006"
      mental <- putInHand p "01089"
      playWith discharge [mental]
      chooseVillain
      pass
      v <- villainIdNow
      selectAny (EnemyWithId (EnemyVillainId v) <> StunnedEnemy) `shouldReturn` False

  it "Lethal Protector removes two threat after an event defeats a minion" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      mid <- addMinion p Minions.weaponsRunner
      placeMainThreat 4
      discharge <- putInHand p "54006"
      mental <- putInHand p "01089"
      playWith discharge [mental]
      choose "attack minion" (isTargetChoice mid)
      choose "Lethal Protector" (abilityOn p 1)
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` 2

  it "Winter Armor adds three hit points and grants steady" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      armor <- putIntoPlay p "54009"
      hitPointsOf p `shouldReturn` 14
      getModifiers p >>= (`shouldSatisfy` elem (KeywordModifier Steady))
      resolve [RemoveFromPlay $ toTarget $ upgradeOf armor]
      hitPointsOf p `shouldReturn` 11

  it "Winter Mask gives the Spy trait while in play" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      mask <- putIntoPlay p "54010"
      identityMatches (IdentityWithTrait T.Spy) p `shouldReturn` True
      resolve [RemoveFromPlay $ toTarget $ upgradeOf mask]
      identityMatches (IdentityWithTrait T.Spy) p `shouldReturn` False

  it "Winter Rifle adds two attack for one basic attack and exhausts" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      rifle <- putIntoPlay p "54011"
      choose "basic attack" (isHeroAttack p)
      choose "Rifle" (abilityOn (upgradeOf rifle) 1)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 4
      upgradeIsExhausted (upgradeOf rifle) `shouldReturn` True
      resolve [IdentityMessage p ReadiedIdentity]
      choose "second attack" (isHeroAttack p)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 6

  it "Black Widow returns an Attack event from discard after being played" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      event <- putInDiscard p "54005"
      widow <- putInHand p "54003"
      resources <- traverse (putInHand p) ["01088", "01086"]
      playWith widow resources
      choose "Black Widow" (abilityOn (allyIdOf widow) 1)
      choose "return attack" (isTargetChoice event)
      pass
      handOf p >>= (`shouldSatisfy` elem event)

  it "Bucky spends one resource to put Cybernetic Arm into play from discard" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      clearHand p
      arm <- putInDiscard p "54002"
      resource <- putInHand p "01086"
      choose "Cybernetically Enhanced" (abilityOn p 1)
      payWith [resource]
      choose "find Arm" (isTargetChoice arm)
      pass
      upgradeCodes >>= (`shouldSatisfy` elem "54002")
      offers (abilityOn p 1) `shouldReturn` False

  it "Cybernetic Arm funds an Attack event and adds one damage" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      arm <- putIntoPlay p "54002"
      punch <- putInHand p "54005"
      energy <- putInHand p "01088"
      choose "Metal Punch" (isPlay punch)
      choose "Arm resource" (abilityOn (upgradeOf arm) 1)
      declareWildAs Physical
      payWith [energy]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 8
      upgradeIsExhausted (upgradeOf arm) `shouldReturn` True

  it "Arm Block deals three damage without preventing an attack when not arm-funded" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      mid <- addMinion p Minions.hydraSoldier
      block <- putInHand p "54004"
      resource <- putInHand p "01086"
      runNow [MinionMessage mid $ MinionAttacks p]
      choose "Arm Block" (isPlay block)
      payWith [resource]
      pass
      minionDamages `shouldReturn` [3]
      damageOf p `shouldReturn` 2

  it "Safe House draws after finding a minion in the encounter deck" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      clearHand p
      house <- putIntoPlay p "54007"
      cards <- stackEncounterDeck [Minions.hydraSoldier]
      choose "Safe House" (isAbilityOfCard house)
      choose "put minion into play" (isTargetChoice $ testHead cards)
      pass
      length <$> handOf p `shouldReturn` 1
      selectAny (MinionIs Minions.hydraSoldier <> MinionEngagedWith (IdentityWithId p)) `shouldReturn` True

  it "Silent Infiltration readies its hero and confuses after a defeated enemy" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      preparation <- putIntoPlay p "54008"
      mid <- addMinion p Minions.weaponsRunner
      choose "attack" (isHeroAttack p)
      choose "minion" (isTargetChoice mid)
      choose "Silent Infiltration" (abilityOn (upgradeOf preparation) 1)
      chooseVillain
      pass
      isExhaustedNow p `shouldReturn` False
      v <- villainIdNow
      selectAny (EnemyWithId (EnemyVillainId v) <> ConfusedEnemy) `shouldReturn` True

  it "Cybernetic Arm cannot generate a resource for a support" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      arm <- putIntoPlay p "54002"
      house <- putInHand p "54007"
      _ <- putInHand p "01086"
      choose "play support" (isPlay house)
      offers (abilityOn (upgradeOf arm) 1) `shouldReturn` False

  it "Winter Armor's steady leaves one stun ineffective and consumes two on the next attack" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      _ <- putIntoPlay p "54009"
      resolve [IdentityMessage p IdentityStunned]
      choose "attack with one stun" (isHeroAttack p)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 2
      resolve [IdentityMessage p ReadiedIdentity, IdentityMessage p IdentityStunned]
      choose "attack with two stuns" (isHeroAttack p)
      pass
      villainDamageNow `shouldReturn` 2
      isStunned p `shouldReturn` False

  it "Black Widow's retrieval does not trigger when she is merely put into play" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      event <- putInDiscard p "54005"
      widow <- putIntoPlay p "54003"
      offers (abilityOn (allyIdOf widow) 1) `shouldReturn` False
      discardOf p >>= (`shouldSatisfy` elem event)

  it "Silent Infiltration and Winter Mask do not trigger for an enemy that survives" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      asWinter p
      preparation <- putIntoPlay p "54008"
      mask <- putIntoPlay p "54010"
      choose "attack villain" (isHeroAttack p)
      chooseVillain
      pass
      offers (abilityOn (upgradeOf preparation) 1) `shouldReturn` False
      offers (abilityOn (upgradeOf mask) 1) `shouldReturn` False
      isExhaustedNow p `shouldReturn` True

  it "Arm Block funded by Cybernetic Arm prevents the surviving attacker's damage" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.fw54028
      asWinter p
      arm <- putIntoPlay p "54002"
      block <- putInHand p "54004"
      runNow [MinionMessage mid $ MinionAttacks p]
      choose "Arm Block" (isPlay block)
      choose "Arm resource" (abilityOn (upgradeOf arm) 1)
      declareWildAs Physical
      payWith []
      pass
      getEnemyRemainingHitPoints (EnemyMinionId mid) `shouldReturn` 1
      damageOf p `shouldReturn` 0
