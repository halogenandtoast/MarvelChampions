module Marvel.Hero.PhoenixSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability.Types qualified as Ability
import Marvel.Ally.Types qualified as Ally
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity (currentIdentity, getModifiedKeywords, PlayerIdentitySide (..))
import Marvel.Keyword (Keyword (Steady, Retaliate))
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Stats
import Marvel.Hp (HP (..))
import Marvel.Phase
import Marvel.Query
import Marvel.Trait (Trait (Aerial, Restrained, Unleashed))
import TestImport

forceNow :: GameAppT (Attrs Upgrade)
forceNow = do
  fs <- (<>) <$> upgradesWith "34002a" <*> upgradesWith "34002b"
  case fs of
    [f] -> pure f
    _ -> error "expected one Phoenix Force"

powerNow :: GameAppT Natural
powerNow = upgradeUses <$> forceNow

forceCode :: GameAppT CardCode
forceCode = toCardCode <$> forceNow

asPhoenix :: IdentityId -> GameAppT ()
asPhoenix p = changeForm p >> settleForced >> clearHand p

identityAbility :: IdentityId -> Natural -> Choice -> Bool
identityAbility p index = \case
  UseAbility ab -> abilitySource ab == IdentityRef p && abilityIndex ab == index
    && ChangeForm `notElem` abilityChoices ab
  _ -> False

phoenixStats :: IdentityId -> GameAppT (Natural, Natural, Natural)
phoenixStats p = currentIdentity <$> getPlayer p >>= \case
  HeroSide h -> (,,) <$> Hero.getModifiedThwart (toAttrs h) <*> Hero.getModifiedAttack (toAttrs h) <*> Hero.getModifiedDefense (toAttrs h)
  _ -> error "expected Phoenix hero"

payDouble :: IdentityId -> CardCode -> GameAppT PlayerCard
payDouble p code = do
  c <- putInHand p code
  energy <- putInHand p "01088"
  playWith c [energy]
  pass
  pure c

spec :: Spec
spec = describe "Phoenix" do
  it "starts as Jean Grey with nine hit points, six-card hand size and permanent restrained Force with four counters" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      hitPointsOf p `shouldReturn` 9
      handSizeOf p `shouldReturn` 6
      powerNow `shouldReturn` 4
      forceCode `shouldReturn` "34002a"
      identityMatches (IdentityWithTrait Restrained) p `shouldReturn` True
      (map toCardCode <$> handOf p) >>= \cs -> cs `shouldSatisfy` notElem "34002a"
      f <- forceNow
      resolve [RemoveFromPlay (UpgradeRef $ upgradeId f)]
      forceCode `shouldReturn` "34002a"

  it "has printed three THW, one ATK and two DEF in restrained hero form" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      asPhoenix p
      phoenixStats p `shouldReturn` (3, 1, 2)
      handSizeOf p `shouldReturn` 5

  it "spends a power counter for a wild resource once per phase" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      asPhoenix p
      firebird <- putInHand p "34013"
      choose "play Phoenix Firebird" (isPlay firebird)
      choose "Psionic Bond pays" (identityAbility p 1)
      payWith []
      choose "place two power" (isLabel "$label.phoenixFirebirdPower")
      powerNow `shouldReturn` 5
      second <- putInHand p "34013"
      offers (isPlay second) `shouldReturn` False
      start [BeginPhase PlayerPhase]
      offers (isPlay second) `shouldReturn` True
      choose "play Firebird in the next phase" (isPlay second)
      choose "Psionic Bond is available again" (identityAbility p 1)
      payWith []
      choose "place two more power" (isLabel "$label.phoenixFirebirdPower")
      powerNow `shouldReturn` 6

  it "cannot generate Psionic Bond resources in alter-ego form" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      clearHand p
      room <- putInHand p "34004"
      offers (isPlay room) `shouldReturn` False
      powerNow `shouldReturn` 4

  it "Jean's basic recovery heals three and optionally adds one power" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      setDamage p 5
      choose "recover" \case UseAbility ab -> Recover `elem` abilityChoices ab; _ -> False
      choose "Jean's response" (identityAbility p 2)
      damageOf p `shouldReturn` 2
      powerNow `shouldReturn` 5

  it "may decline Jean's recovery response and does not respond to another heal" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      setDamage p 5
      choose "recover" \case UseAbility ab -> Recover `elem` abilityChoices ab; _ -> False
      settleForced
      powerNow `shouldReturn` 4
      runNow [IdentityMessage p (IdentityHealed 1)]
      offers (identityAbility p 2) `shouldReturn` False
      powerNow `shouldReturn` 4

  it "flips when the last counter is removed and grants unleashed combat stats and trait" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      asPhoenix p
      f <- forceNow
      resolve [RemoveCounters (UpgradeRef $ upgradeId f) 3]
      forceCode `shouldReturn` "34002a"
      resolve [RemoveCounters (UpgradeRef $ upgradeId f) 1]
      forceCode `shouldReturn` "34002b"
      phoenixStats p `shouldReturn` (1, 3, 2)
      identityMatches (IdentityWithTrait Unleashed) p `shouldReturn` True
      identityMatches (IdentityWithTrait Restrained) p `shouldReturn` False

  it "stays unleashed below four counters, then flips back when a placement reaches four" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      asPhoenix p
      f <- forceNow
      resolve [RemoveCounters (UpgradeRef $ upgradeId f) 4, UpgradeMessage (upgradeId f) (AddUpgradeUses 3)]
      forceCode `shouldReturn` "34002b"
      resolve [UpgradeMessage (upgradeId f) (AddUpgradeUses 1)]
      forceCode `shouldReturn` "34002a"
      powerNow `shouldReturn` 4
      phoenixStats p `shouldReturn` (3, 1, 2)

  it "Cyclops optionally adds two power when played and removes two as he leaves" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      clearHand p
      cyclops <- putInHand p "34003"
      energy <- putInHand p "01088"
      genius <- putInHand p "01089"
      playWith cyclops [energy, genius]
      choose "Cyclops enters" (isAbilityOfCard cyclops)
      powerNow `shouldReturn` 6
      start [RemoveFromPlay (AllyRef $ allyIdOf cyclops)]
      choose "Cyclops leaves" (isAbilityOfCard cyclops)
      powerNow `shouldReturn` 4
      alliesWith "34003" `shouldReturn` []

  it "may decline both Cyclops responses and his printed basic powers have one consequential damage" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      cyclops <- putInHand p "34003"
      energy <- putInHand p "01088"
      genius <- putInHand p "01089"
      playWith cyclops [energy, genius]
      settleForced
      a <- allyIn "34003"
      (Ally.allyThwart a, Ally.allyAttack a, Ally.allyHitPoints a, Ally.allyThwartConsequentialDamage a, Ally.allyAttackConsequentialDamage a)
        `shouldBe` (Thw 2, Atk 2, HP 3, 1, 1)
      start [RemoveFromPlay (AllyRef $ allyIdOf cyclops)]
      settleForced
      powerNow `shouldReturn` 4

  it "White Hot Room exhausts in alter-ego to place one power" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      clearHand p
      room <- payDouble p "34004"
      choose "White Hot Room" (isAbilityOfCard room)
      choose "one power" (isLabel "$label.phoenixPowerOne")
      powerNow `shouldReturn` 5
      offers (isAbilityOfCard room) `shouldReturn` False

  it "White Hot Room can instead heal two damage and cannot be used in hero form" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      setDamage p 4
      room <- payDouble p "34004"
      choose "White Hot Room" (isAbilityOfCard room)
      choose "heal two" (isLabel "$label.phoenixHealTwo")
      damageOf p `shouldReturn` 2
      powerNow `shouldReturn` 4
      asPhoenix p
      resolve [SupportMessage (supportIdOf room) ReadiedSupport]
      offers (isAbilityOfCard room) `shouldReturn` False

  it "Phoenix Suit grants hero Aerial and restrained steady, including in alter-ego" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      _ <- payDouble p "34005"
      identityMatches (IdentityWithTrait Aerial) p `shouldReturn` False
      (getModifiedKeywords =<< getPlayer p) >>= \ks -> ks `shouldSatisfy` elem Steady
      asPhoenix p
      identityMatches (IdentityWithTrait Aerial) p `shouldReturn` True
      runNow [GiveStatus (IdentityRef p) StunnedStatus]
      identityMatches StunnedIdentity p `shouldReturn` False
      runNow [GiveStatus (IdentityRef p) StunnedStatus]
      identityMatches StunnedIdentity p `shouldReturn` True

  it "Phoenix Suit grants retaliate one while unleashed and loses steady" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      _ <- payDouble p "34005"
      asPhoenix p
      f <- forceNow
      resolve [RemoveCounters (UpgradeRef $ upgradeId f) 4]
      ks <- getModifiedKeywords =<< getPlayer p
      ks `shouldSatisfy` elem (Retaliate 1)
      ks `shouldSatisfy` notElem Steady
      mid <- addMinion p Minions.hydraMercenary
      start [MinionMessage mid (MinionAttacks p)]
      settleForced
      minionDamageOf mid `shouldReturn` Just 1

  it "Rise from the Ashes replaces defeat, readies, restores nine hit points and removes all power" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      rise <- putIntoPlay p "34006"
      _ <- putIntoPlay p "05023"
      setDamage p 11
      resolve [IdentityMessage p ExhaustedIdentity]
      start (damageIdentity p (toSource p) $ toDamage 2 FromAbility)
      choose "Rise from the Ashes" (isAbilityOfCard rise)
      settleForced
      damageOf p `shouldReturn` 3
      exhaustedNow p `shouldReturn` False
      powerNow `shouldReturn` 0
      forceCode `shouldReturn` "34002b"
      upgradesWith "34006" `shouldReturn` []
      (map toCardCode <$> discardOf p) >>= \cs -> cs `shouldSatisfy` notElem "34006"

  it "does not offer Rise from the Ashes for nonlethal damage" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      rise <- putIntoPlay p "34006"
      start (damageIdentity p (toSource p) $ toDamage 1 FromAbility)
      offers (isAbilityOfCard rise) `shouldReturn` False
      damageOf p `shouldReturn` 1

  it "Mental Paralysis stops a non-Elite minion's attacks and schemes and discards on the alter-ego flip" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      asPhoenix p
      mid <- addMinion p Minions.weaponsRunner
      paralysis <- putInHand p "34008"
      energy <- putInHand p "01088"
      playOn paralysis mid [energy]
      settleForced
      before <- mainSchemeThreatNow
      resolve [MinionMessage mid (MinionAttacks p), MinionMessage mid MinionSchemes]
      damageOf p `shouldReturn` 0
      mainSchemeThreatNow `shouldReturn` before
      -- The next turn permits another voluntary form change.
      runNow [IdentityEndedTurn p]
      changeForm p
      settleForced
      upgradesWith "34008" `shouldReturn` []
      resolve [MinionMessage mid MinionSchemes]
      mainSchemeThreatNow `shouldReturn` (before + 1)

  it "cannot play Mental Paralysis in alter-ego or attach it to an Elite minion" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      _ <- addMinion p Minions.sandman
      paralysis <- putInHand p "34008"
      _ <- putInHand p "01088"
      offers (isPlay paralysis) `shouldReturn` False
      asPhoenix p
      paralysis' <- putInHand p "34008"
      _ <- putInHand p "01088"
      offers (isPlay paralysis') `shouldReturn` False

  it "does not offer Mind Control when only Elite minions are available" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      _ <- addMinion p Minions.sandman
      control <- putInHand p "34009"
      _ <- putInHand p "01088"
      _ <- putInHand p "01089"
      offers (isPlay control) `shouldReturn` False

  it "Telekinetic Shield absorbs enemy attack damage on its identity, discarding after the entire damage instance reaches five" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      asPhoenix p
      shield <- putInHand p "34007"
      energy <- putInHand p "01088"
      playOn shield p [energy]
      settleForced
      vid <- villainIdNow
      start [VillainMessage vid (VillainAttacksWithoutBoost p)]
      settleForced
      damageOf p `shouldReturn` 0
      [s] <- upgradesWith "34007"
      upgradeUses s `shouldBe` 2
      start [VillainMessage vid (VillainAttacksWithoutBoost p)]
      settleForced
      [s'] <- upgradesWith "34007"
      upgradeUses s' `shouldBe` 4
      start [VillainMessage vid (VillainAttacksWithoutBoost p)]
      settleForced
      damageOf p `shouldReturn` 0
      upgradesWith "34007" `shouldReturn` []
      (map toCardCode <$> discardOf p) >>= \cs -> cs `shouldSatisfy` elem "34007"
      start [VillainMessage vid (VillainAttacksWithoutBoost p)]
      settleForced
      damageOf p `shouldReturn` 2

  it "Telekinetic Shield does not absorb non-attack damage" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      shield <- putInHand p "34007"
      energy <- putInHand p "01088"
      playOn shield p [energy]
      settleForced
      start (damageIdentity p (toSource p) $ toDamage 2 FromAbility)
      settleForced
      damageOf p `shouldReturn` 2
      [s] <- upgradesWith "34007"
      upgradeUses s `shouldBe` 0

  it "Telekinetic Shield can protect another player's ally and absorbs the full lethal attack" do
    tg <- testGame ["phoenix", "spider-man"]
    inGame tg do
      [p, friend] <- players
      ally <- putIntoPlay friend "01083"
      mid <- addMinion p Minions.titania
      resolve [MinionMessage mid (MinionDamaged (toSource p) $ toDamage 3 FromAbility)]
      shield <- putInHand p "34007"
      energy <- putInHand p "01088"
      playOn shield (allyIdOf ally) [energy]
      settleForced
      start [MinionMessage mid (MinionAttacksAlly friend $ allyIdOf ally)]
      settleForced
      a <- allyIn "01083"
      Ally.allyDamage a `shouldBe` 0
      [s] <- upgradesWith "34007"
      upgradeUses s `shouldBe` 3
      start [MinionMessage mid (MinionAttacksAlly friend $ allyIdOf ally)]
      settleForced
      Ally.allyDamage <$> allyIn "01083" >>= (`shouldBe` 0)
      upgradesWith "34007" `shouldReturn` []

  it "Telekinetic Shield does not absorb an attack against another unshielded character" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      asPhoenix p
      ally <- putIntoPlay p "01083"
      shield <- putInHand p "34007"
      energy <- putInHand p "01088"
      playOn shield (allyIdOf ally) [energy]
      settleForced
      mid <- addMinion p Minions.hydraMercenary
      start [MinionMessage mid (MinionAttacks p)]
      settleForced
      damageOf p `shouldReturn` 1
      [s] <- upgradesWith "34007"
      upgradeUses s `shouldBe` 0

  it "does not offer Psionic Bond when no power counters remain" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      asPhoenix p
      f <- forceNow
      resolve [RemoveCounters (UpgradeRef $ upgradeId f) 4]
      firebird <- putInHand p "34013"
      offers (isPlay firebird) `shouldReturn` False

  it "does not offer White Hot Room's healing choice when Jean is undamaged" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      room <- payDouble p "34004"
      choose "White Hot Room" (isAbilityOfCard room)
      offers (isLabel "$label.phoenixHealTwo") `shouldReturn` False
      choose "place power" (isLabel "$label.phoenixPowerOne")
      powerNow `shouldReturn` 5

  it "resolves Phoenix Force's last-counter flip as a forced response during a resource cost, then completes that resource and attack" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      asPhoenix p
      f <- forceNow
      resolve [RemoveCounters (UpgradeRef $ upgradeId f) 3]
      attack <- putInHand p "34010"
      energy <- putInHand p "01088"
      choose "play Telekinetic Attack" (isPlay attack)
      choose "pay the final power counter" (identityAbility p 1)
      powerNow `shouldReturn` 0
      forceCode `shouldReturn` "34002a"
      choose "forced Phoenix Force flip" \case
        UseAbility ab -> abilitySource ab == UpgradeRef (upgradeId f) && abilityIndex ab == 1
        _ -> False
      forceCode `shouldReturn` "34002b"
      payWith [energy]
      before <- villainDamageNow
      chooseVillain
      villainDamageNow `shouldReturn` (before + 9)

  it "updates a placed fourth power before offering the forced restrained flip" do
    tg <- testGame ["phoenix"]
    inGame tg do
      f <- forceNow
      resolve [RemoveCounters (UpgradeRef $ upgradeId f) 4]
      start [UpgradeMessage (upgradeId f) (AddUpgradeUses 4)]
      powerNow `shouldReturn` 4
      forceCode `shouldReturn` "34002b"
      choose "forced Phoenix Force flip" \case
        UseAbility ab -> abilitySource ab == UpgradeRef (upgradeId f) && abilityIndex ab == 2
        _ -> False
      forceCode `shouldReturn` "34002a"
      powerNow `shouldReturn` 4

  it "Cyclops cannot respond to leaving play when Phoenix Force has no counters to remove" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      cyclops <- putIntoPlay p "34003"
      f <- forceNow
      resolve [RemoveCounters (UpgradeRef $ upgradeId f) (upgradeUses f)]
      start [RemoveFromPlay $ AllyRef $ allyIdOf cyclops]
      offers (isAbilityOfCard cyclops) `shouldReturn` False
      powerNow `shouldReturn` 0

-- Mandatory responses are choices too; finish them before observing their effects.
settleForced :: GameAppT ()
settleForced = do
  pass
  forced <- offers isForced
  if forced then choose "mandatory response" isForced >> settleForced else pure ()
 where
  isForced (UseAbility ab) = Ability.abilityType ab `elem` [Ability.ForcedResponse, Ability.ForcedInterrupt]
  isForced _ = False
