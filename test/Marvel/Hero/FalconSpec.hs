module Marvel.Hero.FalconSpec (spec) where

import TestImport
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Minion.Cards qualified as Minions
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Modifier
import Marvel.Query

asFalcon :: IdentityId -> GameAppT ()
asFalcon p = changeForm p >> pass >> clearHand p

abilityOn :: IsRef a => a -> Natural -> Choice -> Bool
abilityOn target n = \case
  UseAbility ab -> abilitySource ab == toSource target && abilityIndex ab == n
  _ -> False

spec :: Spec
spec = describe "Falcon" do
  it "starts with ten hit points and a six-card alter-ego hand" do
    tg <- testGame ["falcon"]
    inGame tg do
      hitPointsOf (player1 tg) `shouldReturn` 10
      handSizeOf (player1 tg) `shouldReturn` 6
      asFalcon (player1 tg)
      handSizeOf (player1 tg) `shouldReturn` 5

  it "Bird of Prey deals four damage without discarding the encounter top" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      attack <- putInHand p "53003"
      energy <- putInHand p "01088"
      playWith attack [energy]
      chooseVillain
      choose "do not discard" (isLabel "$label.doNotDiscard")
      pass
      villainDamageNow `shouldReturn` 4

  it "Bird of Prey counts stars as well as numbered boosts" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      _ <- stackEncounterDeck [Cards.fw53032]
      attack <- putInHand p "53003"
      energy <- putInHand p "01088"
      playWith attack [energy]
      chooseVillain
      choose "discard" (isLabel "$label.discardTopEncounterCard")
      pass
      villainDamageNow `shouldReturn` 5
      map toCardCode <$> encounterDiscardNow >>= (`shouldSatisfy` elem "53032")

  it "Bird's-Eye View removes three plus all boost-area icons" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      placeMainThreat 6
      _ <- stackEncounterDeck [Cards.fw53038]
      thwart <- putInHand p "53004"
      energy <- putInHand p "01088"
      playWith thwart [energy]
      chooseMainScheme
      choose "discard" (isLabel "$label.discardTopEncounterCard")
      pass
      mainSchemeThreatNow `shouldReturn` 0

  it "Redwing returns to hand and deals damage using every printed icon" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      bird <- putIntoPlay p "53002"
      _ <- stackEncounterDeck [Cards.fw53030]
      choose "Redwing" (abilityOn (allyIdOf bird) 1)
      choose "damage" (isLabel "$label.dealDamage")
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 2
      map toCardCode <$> handOf p >>= (`shouldSatisfy` elem "53002")
      selectAny (AllyWithId $ allyIdOf bird) `shouldReturn` False

  it "Vibranium Microweave adds defense while in play" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      weave <- putIntoPlay p "53013"
      getModifiers p >>= (`shouldSatisfy` elem (DefenseModifier 1))
      resolve [RemoveFromPlay $ toTarget $ upgradeOf weave]
      getModifiers p >>= (`shouldSatisfy` notElem (DefenseModifier 1))

  it "Sam Wilson searches for a Bird only once per round" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      clearHand p
      payment <- putInHand p "01086"
      bird <- putInDiscard p "53002"
      choose "Birds of a Feather" (abilityOn p 1)
      choose "discard card" (isTargetChoice payment)
      choose "find Redwing" (isTargetChoice bird)
      pass
      map toCardCode <$> handOf p >>= (`shouldSatisfy` elem "53002")
      offers (abilityOn p 1) `shouldReturn` False

  it "Aerial Recon gains one persistent recon counter from a dealt encounter card" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      recon <- putIntoPlay p "53009"
      choose "recon" (abilityOn (upgradeOf recon) 1)
      choose "deal to player" (isTargetChoice p)
      pass
      getCountersOn (upgradeOf recon) `shouldReturn` 1
      length <$> dealtTo p `shouldReturn` 1

  it "Serpent Soldier has quickstrike and three hit points" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.fw53032
      getEnemyRemainingHitPoints (EnemyMinionId mid) `shouldReturn` 3

  it "does not offer Redwing's hero action in alter-ego form" do
    tg <- testGame ["falcon"]
    inGame tg do
      bird <- putIntoPlay (player1 tg) "53002"
      _ <- stackEncounterDeck [Minions.hydraSoldier]
      offers (abilityOn (allyIdOf bird) 1) `shouldReturn` False

  it "Eagle-Eyed discards the top encounter card after an Aerial card finishes" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      top <- stackEncounterDeck [Cards.fw53030, Cards.fw53032]
      redwing <- putInHand p "53002"
      energy <- putInHand p "01088"
      playWith redwing [energy]
      choose "Eagle-Eyed" (abilityOn p 1)
      pass
      encounterDiscardNow >>= (`shouldSatisfy` elem (testHead top))

  it "Falcon's Flock spends no more than one bird counter on a physical Aerial card" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      flock <- putIntoPlay p "53006"
      redwing <- putInHand p "53002"
      resource <- putInHand p "01086"
      choose "play Redwing" (isPlay redwing)
      choose "Flock resource" (abilityOn (supportOf flock) 1)
      offers (abilityOn (supportOf flock) 1) `shouldReturn` False
      payWith [resource]
      pass
      getCountersOn (supportOf flock) `shouldReturn` 4
      attack <- putInHand p "53003"
      _ <- putInHand p "01086"
      choose "another Aerial card" (isPlay attack)
      offers (abilityOn (supportOf flock) 1) `shouldReturn` True

  it "Soup Kitchen exhausts Sam, heals his REC, and discounts the next ally by two" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      clearHand p
      soup <- putIntoPlay p "53007"
      setDamage p 4
      choose "Soup Kitchen" (abilityOn (supportOf soup) 1)
      pass
      damageOf p `shouldReturn` 1
      isExhaustedNow p `shouldReturn` True
      aero <- putInHand p "53015"
      resource <- putInHand p "01086"
      playWith aero [resource]
      pass
      selectAny (AllyWithId $ allyIdOf aero) `shouldReturn` True

  it "Battlefield Awareness adds every boost-area icon to one basic power" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      awareness <- putIntoPlay p "53010"
      _ <- stackEncounterDeck [Cards.fw53030]
      choose "basic attack" (isHeroAttack p)
      choose "Battlefield Awareness" (abilityOn (upgradeOf awareness) 1)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 4
      upgradeIsExhausted (upgradeOf awareness) `shouldReturn` True

  it "Talon Line readies a character and stuns using Eagle-Eyed's discarded icons" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      talon <- putIntoPlay p "53012"
      resolve [IdentityMessage p ExhaustedIdentity]
      _ <- stackEncounterDeck [Cards.fw53030]
      redwing <- putInHand p "53002"
      energy <- putInHand p "01088"
      playWith redwing [energy]
      choose "Eagle-Eyed" (abilityOn p 1)
      choose "Talon Line" (abilityOn (upgradeOf talon) 1)
      choose "ready Falcon" (isTargetChoice p)
      chooseVillain
      pass
      isExhaustedNow p `shouldReturn` False
      v <- villainIdNow
      selectAny (EnemyWithId (EnemyVillainId v) <> StunnedEnemy) `shouldReturn` True
      upgradeCodes >>= (`shouldSatisfy` notElem "53012")

  it "Up, Up, and Away can swap the assigned boost and draws for the current card" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      _ <- setEncounterDeck [Cards.fw53030, Cards.fw53038, Cards.fw53032] []
      event <- putInHand p "53005"
      v <- villainIdNow
      runNow [VillainMessage v $ VillainAttacks p]
      choose "Up, Up, and Away" (isPlay event)
      payWith []
      choose "swap boost" (isLabel "$label.swapBoostCards")
      length <$> handOf p `shouldReturn` 4

  it "Aerial Evacuation saves an ally, discards itself, and changes Falcon to alter-ego" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      evacuation <- putIntoPlay p "53008"
      bird <- putIntoPlay p "53002"
      mid <- addMinion p Minions.hydraSoldier
      runNow [MinionMessage mid $ MinionAttacks p]
      choose "defend with Redwing" (\case AllyDefend aid _ -> aid == allyIdOf bird; _ -> False)
      choose "Aerial Evacuation" (abilityOn (upgradeOf evacuation) 1)
      pass
      identityMatches AlterEgoIdentity p `shouldReturn` True
      allyDamage <$> allyIn "53002" `shouldReturn` 0
      upgradeCodes >>= (`shouldSatisfy` notElem "53008")

  it "Aerial Evacuation prevents overkill from reaching Falcon when saving the defender" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      evacuation <- putIntoPlay p "53008"
      bird <- putIntoPlay p "53002"
      charge <- revealNow p Attachments.charge
      pass
      v <- villainIdNow
      runNow [VillainMessage v $ VillainAttacksWithoutBoost p]
      choose "Charge" (isAbilityOf $ AttachmentId $ unCardId $ ecCardId charge)
      choose "defend with Redwing" (\case AllyDefend aid _ -> aid == allyIdOf bird; _ -> False)
      choose "Aerial Evacuation" (abilityOn (upgradeOf evacuation) 1)
      pass
      damageOf p `shouldReturn` 0
      allyDamage <$> allyIn "53002" `shouldReturn` 0
      identityMatches AlterEgoIdentity p `shouldReturn` True

  it "Draw Their Fire permits several defenses without exhausting during the villain phase" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      preparation <- putIntoPlay p "53011"
      mid <- addMinion p Minions.hydraSoldier
      choose "end turn" isEndTurn
      choose "Draw Their Fire" (abilityOn (upgradeOf preparation) 1)
      choose "defend villain" isDefend
      isExhaustedNow p `shouldReturn` False
      choose "activate the minion" (isTargetChoice mid)
      offers isDefend `shouldReturn` True
      choose "defend minion" isDefend
      upgradeCodes >>= (`shouldSatisfy` notElem "53011")

  it "Redwing cannot be triggered against a visible zero-icon encounter top" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      bird <- putIntoPlay p "53002"
      _ <- stackEncounterDeck [Attachments.armoredRhinoSuit]
      offers (abilityOn (allyIdOf bird) 1) `shouldReturn` False

  it "Falcon's Flock cannot pay for a card without Aerial" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      flock <- putIntoPlay p "53006"
      soup <- putInHand p "53007"
      _ <- putInHand p "01086"
      choose "play Soup Kitchen" (isPlay soup)
      offers (abilityOn (supportOf flock) 1) `shouldReturn` False

  it "Redwing counts encounter-wide boost modifiers without treating a star as a boost icon" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      _ <- revealNow p Schemes.theNextEvolution
      pass
      bird <- putIntoPlay p "53002"
      _ <- stackEncounterDeck [Cards.fw53032]
      choose "Redwing" (abilityOn (allyIdOf bird) 1)
      choose "damage" (isLabel "$label.dealDamage")
      chooseVillain
      pass
      -- The Serpent Soldier has one star, plus the scheme adds one boost icon.
      villainDamageNow `shouldReturn` 2
