module Marvel.Hero.ColossusSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity ( currentIdentity )
import Marvel.Status
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import TestImport

toughCount :: IdentityId -> GameAppT Natural
toughCount p = do
  a <- playerAttrs p
  pure $ (if a.playerIdentityTough then 1 else 0) + a.playerIdentityAdditionalTough

heroAbility :: IdentityId -> Natural -> Choice -> Bool
heroAbility p n = \case
  UseAbility ab -> abilitySource ab == IdentityRef p && abilityIndex ab == n
  _ -> False

asColossus :: IdentityId -> GameAppT ()
asColossus p = changeForm p >> pass >> clearHand p

spec :: Spec
spec = describe "Colossus" do
  it "starts as Piotr with 14 hit points and Organic Steel in his starting hand" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      hitPointsOf p `shouldReturn` 14
      handSizeOf p `shouldReturn` 6
      cards <- handOf p
      map toCardCode cards `shouldSatisfy` elem "32006"

  it "Steel Skin offers a tough after changing to hero" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      clearHand p
      changeForm p
      offers (heroAbility p 1) `shouldReturn` True
      choose "Steel Skin" (heroAbility p 1)
      toughCount p `shouldReturn` 1

  it "holds two tough cards and spends only one to prevent a damage instance" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      asColossus p
      runNow [GiveStatus (IdentityRef p) ToughStatus, GiveStatus (IdentityRef p) ToughStatus, GiveStatus (IdentityRef p) ToughStatus]
      toughCount p `shouldReturn` 2
      resolve (damageIdentity p (toSource p) $ toDamage 3 FromAbility)
      toughCount p `shouldReturn` 1
      damageOf p `shouldReturn` 0

  it "Iron Will grants THW and responds to a discarded tough" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      asColossus p
      iron <- putIntoPlay p "32004"
      currentIdentity <$> getPlayer p >>= \case
        HeroSide h -> Hero.getModifiedThwart (toAttrs h) `shouldReturn` 2
        _ -> error "expected hero"
      runNow [GiveStatus (IdentityRef p) ToughStatus]
      before <- length <$> handOf p
      start [RemoveStatus (IdentityRef p) ToughStatus]
      choose "Iron Will" (isAbilityOfCard iron)
      (length <$> handOf p) `shouldReturn` (before + 1)

  it "Titanium Muscles grants one ATK" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      asColossus p
      _ <- putIntoPlay p "32005"
      currentIdentity <$> getPlayer p >>= \case
        HeroSide h -> Hero.getModifiedAttack (toAttrs h) `shouldReturn` 3
        _ -> error "expected hero"

  it "Bulletproof Protector spends a tough and gains two tough cards" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      asColossus p
      runNow [GiveStatus (IdentityRef p) ToughStatus]
      card <- putInHand p "32009"
      playWith card []
      choose "two tough" (isLabel "$label.bulletproofProtectorTough")
      toughCount p `shouldReturn` 2

  it "Steel Fist deals five damage without requiring a tough" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      asColossus p
      card <- putInHand p "32008"
      strength <- putInHand p "01090"
      playWith card [strength]
      before <- villainDamageNow
      chooseVillain
      villainDamageNow `shouldReturn` (before + 5)

  it "Organic Steel spends one use to replace a discarded tough and exhausts" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      asColossus p
      organic <- putIntoPlay p "32006"
      runNow [GiveStatus (IdentityRef p) ToughStatus]
      start [RemoveStatus (IdentityRef p) ToughStatus]
      choose "Organic Steel" (isAbilityOfCard organic)
      toughCount p `shouldReturn` 1
      offers (isAbilityOfCard organic) `shouldReturn` False

  it "Made of Rage adds six attack and discards one tough" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      asColossus p
      runNow [GiveStatus (IdentityRef p) ToughStatus]
      card <- putInHand p "32007"
      choose "basic attack" (isHeroAttack p)
      playWith card []
      chooseVillain
      villainDamageNow `shouldReturn` 8
      toughCount p `shouldReturn` 0

  it "Piotr's Studio finds a Colossus card rather than an aspect card" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      clearHand p
      studio <- putIntoPlay p "32003"
      _ <- stackDeck p ["32013", "32014", "32008", "32004"]
      choose "Piotr's Studio" (isAbilityOfCard studio)
      cards <- handOf p
      map toCardCode cards `shouldSatisfy` elem "32008"

  it "Armor Up is offered before a villain activation while in alter-ego" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      clearHand p
      armor <- putInHand p "32010"
      _vid <- villainIdNow
      start [IdentityMessage p VillainAndMinionsActivate]
      offers (isPlay armor) `shouldReturn` True

  it "Homesick allows Piotr to exhaust and remove it from the game" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      clearHand p
      _ <- revealNow p Obligations.homesick
      choose "exhaust Piotr" (isLabel "$label.exhaustPiotrRemoveHomesick")
      (.playerIdentityExhausted) <$> playerAttrs p `shouldReturn` True

  it "Titanium Muscles generates two physical resources while Colossus has two tough cards" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      asColossus p
      muscles <- putIntoPlay p "32005"
      runNow [GiveStatus (IdentityRef p) ToughStatus, GiveStatus (IdentityRef p) ToughStatus]
      fist <- putInHand p "32008"
      choose "Steel Fist" (isPlay fist)
      choose "Titanium Muscles pays both resources" (isAbilityOfCard muscles)
      whenM (offers isFinishPayment) $ choose "finish payment" isFinishPayment
      chooseVillain
      choose "keep tough cards" (isLabel "$label.done")
      villainDamageNow `shouldReturn` 5
      toughCount p `shouldReturn` 2

  it "Bulletproof Protector can spend a tough to ready Colossus instead" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      asColossus p
      runNow [GiveStatus (IdentityRef p) ToughStatus, IdentityMessage p ExhaustedIdentity]
      card <- putInHand p "32009"
      playWith card []
      choose "ready Colossus" (isLabel "$label.ready")
      (.playerIdentityExhausted) <$> playerAttrs p `shouldReturn` False
      toughCount p `shouldReturn` 0

  it "Made of Rage grants overkill to the basic attack" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      asColossus p
      runNow [GiveStatus (IdentityRef p) ToughStatus]
      mid <- addMinion p Minions.weaponsRunner
      card <- putInHand p "32007"
      choose "basic attack" (isHeroAttack p)
      playWith card []
      choose "the minion" (isTargetChoice mid)
      villainDamageNow `shouldReturn` 6
      minionDamageOf mid `shouldReturn` Nothing

  it "Armor Up changes a generic activation into an attack before its type is chosen" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      clearHand p
      armor <- putInHand p "32010"
      before <- mainSchemeThreatNow
      start [IdentityMessage p VillainAndMinionsActivate]
      playWith armor []
      choose "Steel Skin" (heroAbility p 1)
      choose "take the attack" isNoDefender
      (.playerIdentitySide) <$> playerAttrs p `shouldReturn` A
      toughCount p `shouldReturn` 0
      mainSchemeThreatNow `shouldReturn` before

  it "Aspiring Artist shuffles a Colossus card, while leaving aspect cards in discard" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      asColossus p
      signature <- putInDiscard p "32008"
      aspect <- putInDiscard p "32013"
      start [IdentityMessage p (ChangedToForm B)]
      choose "Aspiring Artist" (heroAbility p 2)
      offers (isTargetChoice aspect) `shouldReturn` False
      choose "shuffle Steel Fist" (isTargetChoice signature)
      cards <- discardOf p
      map toCardCode cards `shouldSatisfy` notElem "32008"
      map toCardCode cards `shouldSatisfy` elem "32013"

  it "Organic Steel discards itself after its second steel counter is spent" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      asColossus p
      organic <- putIntoPlay p "32006"
      let uid = UpgradeId (unCardId $ pcCardId organic)
      runNow [GiveStatus (IdentityRef p) ToughStatus]
      start [RemoveStatus (IdentityRef p) ToughStatus]
      choose "first steel counter" (isAbilityOfCard organic)
      runNow [UpgradeMessage uid ReadiedUpgrade]
      start [RemoveStatus (IdentityRef p) ToughStatus]
      choose "second steel counter" (isAbilityOfCard organic)
      toughCount p `shouldReturn` 1
      getsGame (HashMap.member uid . gameUpgrades) `shouldReturn` False

  it "Iron Will can respond separately to both tough cards discarded by piercing" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      asColossus p
      iron <- putIntoPlay p "32004"
      runNow [GiveStatus (IdentityRef p) ToughStatus, GiveStatus (IdentityRef p) ToughStatus]
      before <- length <$> handOf p
      vid <- villainIdNow
      start (damageIdentity p (VillainRef vid) ((toDamage 2 FromAttack) {damagePiercing = True}))
      toughCount p `shouldReturn` 0
      damageOf p `shouldReturn` 0
      (length <$> handOf p) `shouldReturn` before
      choose "Iron Will for the first tough" (isAbilityOfCard iron)
      (length <$> handOf p) `shouldReturn` (before + 1)
      damageOf p `shouldReturn` 0
      choose "Iron Will for the second tough" (isAbilityOfCard iron)
      toughCount p `shouldReturn` 0
      damageOf p `shouldReturn` 2
      (length <$> handOf p) `shouldReturn` (before + 2)

  it "Shadowcat ally ignores guard, patrol and crisis independently of Colossus" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      asColossus p
      shadowcat <- putIntoPlay p "32002"
      let aid = AllyId (unCardId $ pcCardId shadowcat)
          power n = \case
            UseAbility ab -> abilitySource ab == AllyRef aid && abilityIndex ab == n
            _ -> False
      _ <- addMinion p Minions.hellfirePawn
      _ <- revealNow p SideSchemes.defenseNetwork
      pass
      placeMainThreat 4
      before <- mainSchemeThreatNow
      choose "Shadowcat attacks past guard" (power 301)
      chooseVillain
      villainDamageNow `shouldReturn` 2
      runNow [AllyMessage aid ReadiedAlly]
      choose "Shadowcat thwarts past patrol and crisis" (power 300)
      sid <- mainSchemeIdNow
      choose "main scheme" (isTargetChoice sid)
      mainSchemeThreatNow `shouldReturn` (before - 2)

  it "Organic Steel's replacement tough prevents the pending piercing damage without piercing again" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      asColossus p
      organic <- putIntoPlay p "32006"
      let uid = UpgradeId (unCardId $ pcCardId organic)
      setDamage p 13
      runNow [GiveStatus (IdentityRef p) ToughStatus]
      vid <- villainIdNow
      start (damageIdentity p (VillainRef vid) ((toDamage 2 FromAttack) {damagePiercing = True}))
      toughCount p `shouldReturn` 0
      damageOf p `shouldReturn` 13
      choose "Organic Steel replaces the pierced tough" (isAbilityOfCard organic)
      damageOf p `shouldReturn` 13
      toughCount p `shouldReturn` 0
      offers (isAbilityOfCard organic) `shouldReturn` False
      exhausted <- getsGame $ maybe False (upgradeExhausted . toAttrs) . HashMap.lookup uid . gameUpgrades
      exhausted `shouldBe` True
