module Marvel.Hero.PsylockeSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.List (nub)
import Marvel.Hero.Runner qualified as Hero
import Marvel.Deck (unDeck)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Identity (PlayerIdentitySide (..), currentIdentity)
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Query
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as V
import TestImport

asPsylocke :: IdentityId -> GameAppT ()
asPsylocke p = clearHand p >> changeForm p >> pass

psiControl :: IdentityId -> Choice -> Bool
psiControl p = \case
  UseAbility ab -> abilitySource ab == IdentityRef p && abilityIndex ab == 1
  _ -> False

weaponIds :: IdentityId -> GameAppT [UpgradeId]
weaponIds p = selectList (UpgradeControlledBy (IdentityWithId p) <> UpgradeOneOf [UpgradeIs Upgrades.psiKnife, UpgradeIs Upgrades.psiKatana])

stats :: IdentityId -> GameAppT (Natural, Natural, Natural)
stats p = currentIdentity <$> getPlayer p >>= \case
  HeroSide h -> (,,) <$> Hero.getModifiedThwart (toAttrs h) <*> Hero.getModifiedAttack (toAttrs h) <*> Hero.getModifiedDefense (toAttrs h)
  _ -> pure (0,0,0)

rhinoAttacks :: IdentityId -> GameAppT ()
rhinoAttacks p = do
  _ <- stackEncounterDeck [Treacheries.imTough]
  vid <- villainIdNow
  runNow [VillainMessage vid (VillainAttacks p)]

spec :: Spec
spec = describe "Psylocke" do
  it "sets up two distinct permanent Psi-Knives outside her forty-card draw deck" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      us <- weaponIds p
      length us `shouldBe` 2
      length (nub us) `shouldBe` 2
      hitPointsOf p `shouldReturn` 10
      knives <- upgradesWith "41002a"
      length knives `shouldBe` 2

  it "gets +1 THW per knife and +1 ATK and piercing per katana" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asPsylocke p
      stats p `shouldReturn` (3,1,2)
      u <- head <$> weaponIds p
      runNow [CardSpecific (UpgradeRef u) "flipPsiEnergy" Null]
      stats p `shouldReturn` (2,2,2)
      us <- weaponIds p
      for_ (filter (/= u) us) $ \other -> runNow [CardSpecific (UpgradeRef other) "flipPsiEnergy" Null]
      stats p `shouldReturn` (1,3,2)

  it "flips one weapon before calculating a basic attack" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asPsylocke p
      choose "basic attack" (isHeroAttack p)
      choose "Psi-Energy Control" (psiControl p)
      u <- head <$> weaponIds p
      choose "flip first knife" (isTargetChoice u)
      chooseVillain
      villainDamageNow `shouldReturn` 2
      length <$> upgradesWith "41002a" `shouldReturn` 1
      length <$> upgradesWith "41002b" `shouldReturn` 1

  it "can decline Psi-Energy Control" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asPsylocke p
      choose "basic attack" (isHeroAttack p)
      pass
      chooseVillain
      villainDamageNow `shouldReturn` 1
      length <$> upgradesWith "41002a" `shouldReturn` 2

  it "recalculates basic thwart after flipping a knife to a katana" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asPsylocke p
      placeMainThreat 5
      before <- mainSchemeThreatNow
      choose "basic thwart" (isHeroThwart p)
      choose "Psi-Energy Control" (psiControl p)
      u <- head <$> weaponIds p
      choose "flip knife" (isTargetChoice u)
      chooseMainScheme
      mainSchemeThreatNow `shouldReturn` (before - 2)

  it "offers Psi-Energy Control during a basic defense" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asPsylocke p
      rhinoAttacks p
      choose "defend" isDefend
      choose "Psi-Energy Control" (psiControl p)
      u <- head <$> weaponIds p
      choose "flip knife" (isTargetChoice u)
      pass
      length <$> upgradesWith "41002b" `shouldReturn` 1

  it "does not trigger her basic-power interrupt when stunned replaces the attack" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asPsylocke p
      resolve [IdentityMessage p IdentityStunned]
      choose "basic attack" (isHeroAttack p)
      offers (psiControl p) `shouldReturn` False
      length <$> upgradesWith "41002a" `shouldReturn` 2

  it "generates a mental resource and optionally flips the exhausted knife" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asPsylocke p
      skill <- putInHand p "41010"
      u <- head <$> weaponIds p
      choose "play skill" (isPlay skill)
      choose "knife resource" (isAbilityOf u)
      choose "flip weapon" (isLabel "$label.flipPsiEnergy")
      whenM (offers isFinishPayment) $ choose "finish payment" isFinishPayment
      upgradeIsExhausted u `shouldReturn` True
      katana <- selectList (UpgradeIs Upgrades.psiKatana)
      katana `shouldBe` [u]

  it "can keep a knife on its current side after generating a resource" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asPsylocke p
      skill <- putInHand p "41010"
      u <- head <$> weaponIds p
      choose "play skill" (isPlay skill)
      choose "knife resource" (isAbilityOf u)
      choose "keep side" (isLabel "$label.done")
      whenM (offers isFinishPayment) $ choose "finish payment" isFinishPayment
      length <$> upgradesWith "41002a" `shouldReturn` 2
      upgradeIsExhausted u `shouldReturn` True

  it "Betsy exhausts a weapon to shuffle a Psionic card from discard into her deck" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      clearHand p
      card <- putInDiscard p "41005"
      choose "Psionic Manifestation action" $ \case
        UseAbility ab -> abilitySource ab == IdentityRef p && abilityIndex ab == 2
        _ -> False
      u <- head <$> weaponIds p
      choose "exhaust weapon" (isTargetChoice u)
      choose "shuffle Psionic" (isTargetChoice card)
      map toCardCode <$> discardOf p `shouldReturn` []
      deck <- unDeck . playerIdentityDeck <$> playerAttrs p
      map pcCardId deck `shouldSatisfy` elem (pcCardId card)
      upgradeIsExhausted u `shouldReturn` True

  it "Angel readies the identity after being played from hand" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asPsylocke p
      resolve [IdentityMessage p ExhaustedIdentity]
      angel <- putInHand p "41003"
      r1 <- putInHand p "01088"
      r2 <- putInHand p "01089"
      playWith angel [r1,r2]
      choose "Angel response" (isAbilityOfCard angel)
      exhaustedNow p `shouldReturn` False

  it "Angel does not offer his ready response when merely put into play" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      angel <- putIntoPlay p "41003"
      offers (\case UseAbility ab -> abilitySource ab == AllyRef (allyIdOf angel) && abilityIndex ab == 1; _ -> False) `shouldReturn` False

  it "Training Regimen searches for a Skill without discarding in alter-ego form" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      clearHand p
      regimen <- putIntoPlay p "41008"
      found <- head <$> stackDeck p ["41009", "41005"]
      choose "Training Regimen" (isAbilityOfCard regimen)
      choose "Skill card" (isTargetChoice found)
      (map toCardCode <$> handOf p) `shouldReturn` ["41009"]

  it "Training Regimen discards a card after searching in hero form" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asPsylocke p
      regimen <- putIntoPlay p "41008"
      found <- head <$> stackDeck p ["41009", "41005"]
      choose "Training Regimen" (isAbilityOfCard regimen)
      choose "Skill card" (isTargetChoice found)
      choose "discard found card" (isTargetChoice found)
      (map toCardCode <$> handOf p) `shouldReturn` []

  it "Martial Arts Training grants DEF and can be discarded to ready after defense" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asPsylocke p
      skill <- putIntoPlay p "41009"
      stats p `shouldReturn` (3,1,3)
      rhinoAttacks p
      choose "defend" isDefend
      choose "decline Psi-Energy Control" isNoResponse
      choose "Martial Arts Training" (isAbilityOfCard skill)
      exhaustedNow p `shouldReturn` False
      upgradeCodes `shouldReturn` ["41002a","41002a"]

  it "Psionic Training ignores guard and patrol and confuses after thwarting" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asPsylocke p
      skill <- putIntoPlay p "41010"
      placeMainThreat 4
      choose "thwart" (isHeroThwart p)
      pass
      chooseMainScheme
      choose "Psionic Training" (isAbilityOfCard skill)
      chooseVillain
      V.villainConfused . toAttrs <$> getVillain `shouldReturn` True

  it "Weapons Training readies both weapons after attacking and discards itself" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asPsylocke p
      skill <- putIntoPlay p "41011"
      us <- weaponIds p
      resolve [UpgradeMessage u ExhaustedUpgrade | u <- us]
      choose "attack" (isHeroAttack p)
      pass
      chooseVillain
      choose "Weapons Training" (isAbilityOfCard skill)
      traverse upgradeIsExhausted us `shouldReturn` [False,False]
      upgradeCodes `shouldReturn` ["41002a","41002a"]

  it "enforces the restricted limit after a flip without offering permanent weapons for discard" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asPsylocke p
      otherWeapon <- putIntoPlay p "06019"
      us <- weaponIds p
      runNow [CardSpecific (UpgradeRef u) "flipPsiEnergy" Null | u <- us]
      for_ us $ \u -> offers (isTargetChoice u) `shouldReturn` False
      choose "discard third restricted card" (isTargetChoice $ upgradeIdOf otherWeapon)
      length <$> upgradesWith "41002b" `shouldReturn` 2
      length <$> upgradesWith "06019" `shouldReturn` 0

  it "Concussive Blow checks Float Like a Butterfly before its own confusion" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asPsylocke p
      _ <- putIntoPlay p "41017"
      event <- putInHand p "41014"
      physical <- putInHand p "01090"
      mental <- putInHand p "01089"
      playWith event [physical,mental]
      chooseVillain
      villainDamageNow `shouldReturn` 3
      V.villainConfused . toAttrs <$> getVillain `shouldReturn` True

  it "Concussive Blow still attacks and provokes retaliation without physical payment" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asPsylocke p
      mid <- addMinion p Minions.whiplash
      event <- putInHand p "41014"
      energy <- putInHand p "01088"
      mental <- putInHand p "01089"
      playWith event [energy,mental]
      choose "confuse Whiplash" (isTargetChoice mid)
      damageOf p `shouldReturn` 1
      minionDamageOf mid `shouldReturn` Just 0
