module Marvel.Aspect.PsylockePackSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally.Types
import Marvel.Deck
import Marvel.Game (getPlayerVictoryDisplay)
import Marvel.Matchers hiding (ExhaustedIdentity, ExhaustedAlly)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Status
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Window qualified as W
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

cardAbility :: PlayerCard -> Natural -> Choice -> Bool
cardAbility card n choice = isAbilityOfCard card choice && case choice of
  UseAbility ab -> abilityIndex ab == n
  _ -> False

villainConfusedNow, villainStunnedNow :: GameAppT Bool
villainConfusedNow = selectAny (VillainEnemy <> ConfusedEnemy)
villainStunnedNow = selectAny (VillainEnemy <> StunnedEnemy)

upgradeIn :: CardCode -> GameAppT (Attrs Upgrade)
upgradeIn code = getsGame $ toAttrs . fromMaybe (error "upgrade missing") . headMay . filter ((== code) . toCardCode . getCardDef) . HashMap.elems . gameUpgrades

supportIn :: CardCode -> GameAppT (Attrs Support)
supportIn code = getsGame $ toAttrs . fromMaybe (error "support missing") . headMay . filter ((== code) . toCardCode . supportCardDef . toAttrs) . HashMap.elems . gameSupports

toVillainAttack :: IdentityId -> GameAppT ()
toVillainAttack _ = do
  endTurn
  untilDefend (20 :: Int)
 where
  untilDefend 0 = error "villain did not attack"
  untilDefend n = whenM (not <$> offers isDefend) $ do
    choose "decline" (isLabel "$label.noResponse")
    untilDefend (n - 1)

spec :: Spec
spec = describe "Psylocke pack" do
  it "Captain Britain takes one consequential damage after thwarting a side scheme" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      captain <- putIntoPlay p "41012"
      sid <- sideSchemeOf <$> revealNow p Schemes.crowdControl
      pass
      choose "Captain Britain thwarts" (cardAbility captain 300)
      choose "side scheme" (isTargetChoice sid)
      pass
      allyDamage <$> allyIn "41012" `shouldReturn` 1

  it "Captain Britain takes two consequential damage against the villain" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      captain <- putIntoPlay p "41012"
      choose "Captain Britain attacks" (cardAbility captain 301)
      chooseVillain
      pass
      allyDamage <$> allyIn "41012" `shouldReturn` 2
      villainDamageNow `shouldReturn` 3

  it "Captain Britain retains his consequential reduction after defeating a minion" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      captain <- putIntoPlay p "41012"
      mid <- addMinion p Minions.hydraSoldier
      resolve [MinionMessage mid (MinionDamaged (toSource p) $ toDamage 1 FromAbility)]
      choose "Captain Britain attacks" (cardAbility captain 301)
      choose "minion" (isTargetChoice mid)
      pass
      allyDamage <$> allyIn "41012" `shouldReturn` 1

  it "Cypher draws after damaging a confused enemy" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      cypher <- putIntoPlay p "41013"
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainConfused GameRef)]
      choose "Cypher attacks" (cardAbility cypher 301)
      chooseVillain
      choose "Cypher response" (cardAbility cypher 1)
      pass
      length <$> handOf p `shouldReturn` 1
      villainDamageNow `shouldReturn` 1

  it "Cypher does not respond to an unconfused target or damage stopped by tough" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      cypher <- putIntoPlay p "41013"
      choose "Cypher attacks" (cardAbility cypher 301)
      chooseVillain
      offers (cardAbility cypher 1) `shouldReturn` False
      pass
      vid <- villainIdNow
      resolve [AllyMessage (allyIdOf cypher) ReadiedAlly, VillainMessage vid (VillainConfused GameRef), GiveStatus (VillainRef vid) ToughStatus]
      choose "Cypher attacks tough" (cardAbility cypher 301)
      chooseVillain
      offers (cardAbility cypher 1) `shouldReturn` False

  it "Upside the Head confuses an enemy damaged by a basic attack" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putInHand p "41015"
      pay <- putInHand p "01091"
      choose "attack" (isHeroAttack p)
      pass
      chooseVillain
      playWith card [pay]
      chooseVillain
      pass
      villainConfusedNow `shouldReturn` True

  it "Upside the Head stuns instead when the damaged enemy is already confused" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainConfused GameRef)]
      card <- putInHand p "41015"
      pay <- putInHand p "01091"
      choose "attack" (isHeroAttack p)
      pass
      chooseVillain
      playWith card [pay]
      chooseVillain
      pass
      villainStunnedNow `shouldReturn` True
      villainConfusedNow `shouldReturn` True

  it "Upside the Head is unavailable after a basic attack deals no damage" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putInHand p "41015"
      _ <- putInHand p "01091"
      vid <- villainIdNow
      resolve [GiveStatus (VillainRef vid) ToughStatus]
      choose "attack" (isHeroAttack p)
      pass
      chooseVillain
      offers (isPlay card) `shouldReturn` False

  it "Lay the Trap deals five damage per player from the defeating player and enters victory display" do
    tg <- testGame ["psylocke", "spider-man"]
    inGame tg do
      let p = player1 tg; other = player2 tg
      asHero p
      trap <- putIntoPlay p "41016"
      let sid = SideSchemeId $ unCardId $ pcCardId trap
      runNow [SideSchemeMessage sid (SideSchemeThwarted (toSource other) 6)]
      pressWhenRevealed
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 10
      map toCardCode <$> getPlayerVictoryDisplay >>= (`shouldSatisfy` elem "41016")

  it "Float Like a Butterfly boosts a hero attack against an already confused enemy" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      butterfly <- putIntoPlay p "41017"
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainConfused GameRef)]
      choose "attack" (isHeroAttack p)
      pass
      chooseVillain
      choose "Butterfly" (cardAbility butterfly 1)
      pass
      villainDamageNow `shouldReturn` 2

  it "Float Like a Butterfly boosts a controlled ally and is optional" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      butterfly <- putIntoPlay p "41017"
      angel <- putIntoPlay p "41003"
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainConfused GameRef)]
      choose "Angel attacks" (cardAbility angel 301)
      chooseVillain
      choose "Butterfly" (cardAbility butterfly 1)
      pass
      villainDamageNow `shouldReturn` 3
      resolve [AllyMessage (allyIdOf angel) ReadiedAlly]
      choose "Angel attacks again" (cardAbility angel 301)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 5

  it "Float Like a Butterfly boosts every damage instance against the same initially confused enemy" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      weapon : _ <- upgradesOf p
      resolve [CardSpecific (UpgradeRef weapon) "flipPsiEnergy" (toJSON ())]
      butterfly <- putIntoPlay p "41017"
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainConfused GameRef)]
      flurry <- putInHand p "41004"
      pay <- putInHand p "01089"
      extra <- putInHand p "01091"
      playWith flurry [pay,extra]
      chooseVillain
      choose "Butterfly" (cardAbility butterfly 1)
      chooseVillain
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 6

  it "Float Like a Butterfly does not activate when the same attack confuses its target" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      weapon : _ <- upgradesOf p
      resolve [CardSpecific (UpgradeRef weapon) "flipPsiEnergy" (toJSON ())]
      butterfly <- putIntoPlay p "41017"
      flurry <- putInHand p "41004"
      pay <- putInHand p "01089"
      extra <- putInHand p "01091"
      playWith flurry [pay,extra]
      chooseVillain
      chooseVillain
      offers (cardAbility butterfly 1) `shouldReturn` False
      chooseVillain
      offers (cardAbility butterfly 1) `shouldReturn` False
      pass
      villainDamageNow `shouldReturn` 4

  it "Pete Wisdom heals one damage after his controller resolves a treachery" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      pete <- putIntoPlay p "41018"
      resolve [AllyMessage (allyIdOf pete) (AllyDamaged (toSource p) $ toDamage 2 FromAbility)]
      _ <- revealNow p Treacheries.hardToKeepDown
      choose "Pete Wisdom" (cardAbility pete 1)
      pass
      allyDamage <$> allyIn "41018" `shouldReturn` 1

  it "Domino swaps a chosen card with the top of the deck after a basic power" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      domino <- putIntoPlay p "41031"
      hand <- putInHand p "01091"
      [top] <- stackDeck p ["41004"]
      choose "Domino attacks" (cardAbility domino 301)
      chooseVillain
      choose "Domino response" (cardAbility domino 1)
      choose "swap hand card" (isTargetChoice hand)
      pass
      handOf p `shouldReturn` [top]
      unDeck . playerIdentityDeck <$> playerAttrs p >>= ((`shouldBe` hand) . head)

  it "Psi-Bow Attack deals four ranged damage without taking retaliate" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      bow <- putInHand p "41030"
      pay <- putInHand p "01089"
      playWith bow [pay]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 4
      damageOf p `shouldReturn` 0

  it "Directed Force adds two damage to a ranged event attack" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      bow <- putInHand p "41030"
      force <- putInHand p "41019"
      pay <- putInHand p "01089"
      playWith bow [pay]
      chooseVillain
      playWith force []
      pass
      villainDamageNow `shouldReturn` 6

  it "Directed Force cannot interrupt an attack without a qualifying keyword" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      force <- putInHand p "41019"
      choose "attack" (isHeroAttack p)
      pass
      chooseVillain
      offers (isPlay force) `shouldReturn` False

  it "Soaring Hearts retrieves a signature event and readies Angel and Psylocke" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      angel <- putIntoPlay p "41003"
      event <- putInHand p "41004"
      resolve [DiscardedCard $ PlayerCard event, IdentityMessage p ExhaustedIdentity, AllyMessage (allyIdOf angel) ExhaustedAlly]
      hearts <- putInHand p "41020"
      pay <- putInHand p "01089"
      playWith hearts [pay]
      choose "signature event" (isTargetChoice event)
      pass
      handOf p >>= (`shouldSatisfy` elem event)
      exhaustedNow p `shouldReturn` False
      allyExhausted <$> allyIn "41003" `shouldReturn` False

  it "Soaring Hearts needs Angel in play" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      hearts <- putInHand p "41020"
      _ <- putInHand p "01089"
      offers (isPlay hearts) `shouldReturn` False

  it "IPAC deals an encounter card to the chosen player who draws two cards" do
    tg <- testGame ["psylocke", "spider-man"]
    inGame tg do
      let p = player1 tg; other = player2 tg
      asHero p
      clearHand other
      ipac <- putIntoPlay p "41022"
      before <- length <$> dealtTo other
      choose "IPAC" (cardAbility ipac 1)
      choose "other player" (isTargetChoice other)
      pass
      length <$> dealtTo other `shouldReturn` (before + 1)
      length <$> handOf other `shouldReturn` 2
      supportExhausted <$> supportIn "41022" `shouldReturn` True

  it "X-Bunker searches as many top cards as victory side schemes for a Mutant player" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      clearHand p
      bunker <- putIntoPlay p "41023"
      scheme <- putIntoPlay p "40018"
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      resolve [SideSchemeMessage sid (SideSchemeThwarted (toSource p) 3)]
      [top] <- stackDeck p ["41004"]
      choose "X-Bunker" (cardAbility bunker 1)
      choose "Betsy" (isTargetChoice p)
      choose "top card" (isTargetChoice top)
      pass
      handOf p >>= (`shouldSatisfy` elem top)

  it "X-Bunker cannot be used without a Mutant identity in play" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      bunker <- putIntoPlay p "41023"
      offers (cardAbility bunker 1) `shouldReturn` False

  it "Telepathy exhausts and spends two mental resources to remove two threat" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      telepathy <- putIntoPlay p "41024"
      pay <- putInHand p "01089"
      placeMainThreat 5
      before <- mainSchemeThreatNow
      choose "Telepathy" (cardAbility telepathy 1)
      payWith [pay]
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` (before - 2)
      upgradeExhausted <$> upgradeIn "41024" `shouldReturn` True

  it "Telekinesis exhausts and spends two mental resources to attack for three" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      telekinesis <- putIntoPlay p "41033"
      pay <- putInHand p "01089"
      choose "Telekinesis" (cardAbility telekinesis 1)
      payWith [pay]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 3
      upgradeExhausted <$> upgradeIn "41033" `shouldReturn` True

  it "Psi-Flail Strike damages and stuns the enemy after defending its attack" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      strike <- putInHand p "41032"
      pay <- putInHand p "01089"
      toVillainAttack p
      choose "defend" isDefend
      choose "decline weapon flip" (isLabel "$label.noResponse")
      playWith strike [pay]
      pass
      villainDamageNow `shouldReturn` 3
      villainStunnedNow `shouldReturn` True

  it "Soaring Hearts still readies both characters when the discard pile has no signature event" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      angel <- putIntoPlay p "41003"
      resolve [IdentityMessage p ExhaustedIdentity, AllyMessage (allyIdOf angel) ExhaustedAlly]
      hearts <- putInHand p "41020"
      pay <- putInHand p "01089"
      playWith hearts [pay]
      pass
      exhaustedNow p `shouldReturn` False
      allyExhausted <$> allyIn "41003" `shouldReturn` False

  it "Telepathy permits Psionic alter-egos but Telekinesis requires a Psionic hero" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      clearHand p
      telepathy <- putInHand p "41024"
      telekinesis <- putInHand p "41033"
      _ <- putInHand p "01091"
      offers (isPlay telepathy) `shouldReturn` True
      offers (isPlay telekinesis) `shouldReturn` False

  it "Telepathy cannot be played by an identity without Psionic" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      clearHand p
      telepathy <- putInHand p "41024"
      _ <- putInHand p "01091"
      offers (isPlay telepathy) `shouldReturn` False

  it "Telepathy has a maximum of one copy per player" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "41024"
      second <- putInHand p "41024"
      _ <- putInHand p "01091"
      offers (isPlay second) `shouldReturn` False

  it "Float Like a Butterfly may be played under another player's control" do
    tg <- testGame ["psylocke", "spider-man"]
    inGame tg do
      let p = player1 tg; other = player2 tg
      asHero p
      butterfly <- putInHand p "41017"
      pay <- putInHand p "01089"
      playOn butterfly other [pay]
      pass
      upgradeController <$> upgradeIn "41017" `shouldReturn` other

  it "The Power of the Mind doubles when paying Telepathy's Psionic ability cost" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      weapons <- upgradesOf p
      resolve [UpgradeMessage uid ExhaustedUpgrade | uid <- weapons]
      telepathy <- putIntoPlay p "41024"
      pay <- putInHand p "41021"
      placeMainThreat 5
      before <- mainSchemeThreatNow
      choose "Telepathy" (cardAbility telepathy 1)
      payWith [pay]
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` (before - 2)

  it "Directed Force is limited to one copy across distinct keyword targets in one Flurry attack" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      weapon : _ <- upgradesOf p
      resolve [CardSpecific (UpgradeRef weapon) "flipPsiEnergy" (toJSON ())]
      first <- addMinion p Minions.tombstone
      second <- addMinion p Minions.radioactiveMan
      mark1 <- putInHand p "33032"
      playOn mark1 first []
      mark2 <- putInHand p "33032"
      playOn mark2 second []
      flurry <- putInHand p "41004"
      force <- putInHand p "41019"
      extraForce <- putInHand p "41019"
      pay <- putInHand p "01089"
      extra <- putInHand p "01091"
      playWith flurry [pay,extra]
      choose "first marked target" (isTargetChoice first)
      playWith force []
      chooseVillain
      choose "second marked target" (isTargetChoice second)
      offers (isPlay extraForce) `shouldReturn` False
      pass
      minionDamageOf first `shouldReturn` Just 4
      minionDamageOf second `shouldReturn` Just 2
      villainDamageNow `shouldReturn` 0

  it "Directed Force can trigger on a later Marked target and boosts only that target" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      weapon : _ <- upgradesOf p
      resolve [CardSpecific (UpgradeRef weapon) "flipPsiEnergy" (toJSON ())]
      marked <- addMinion p Minions.tombstone
      mark <- putInHand p "33032"
      playOn mark marked []
      flurry <- putInHand p "41004"
      force <- putInHand p "41019"
      pay <- putInHand p "01089"
      extra <- putInHand p "01091"
      playWith flurry [pay,extra]
      chooseVillain
      offers (isPlay force) `shouldReturn` False
      chooseVillain
      choose "marked target" (isTargetChoice marked)
      playWith force []
      pass
      villainDamageNow `shouldReturn` 2
      minionDamageOf marked `shouldReturn` Just 4

  it "Upside the Head can confuse the villain damaged by a basic attack's overkill" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "01057"
      mid <- addMinion p Minions.hydraSoldier
      resolve [MinionMessage mid (MinionDamaged (toSource p) $ toDamage 3 FromAbility)]
      mark <- putInHand p "33032"
      playOn mark mid []
      upside <- putInHand p "41015"
      pay <- putInHand p "01091"
      choose "basic attack" (isHeroAttack p)
      pass
      choose "wounded minion" (isTargetChoice mid)
      playWith upside [pay]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 1
      villainConfusedNow `shouldReturn` True

  it "Cypher responds to overkill damage against a confused villain" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      cypher <- putIntoPlay p "41013"
      inspired <- putInHand p "01074"
      pay <- putInHand p "01091"
      playOn inspired (allyIdOf cypher) [pay]
      mid <- addMinion p Minions.hydraSoldier
      resolve [MinionMessage mid (MinionDamaged (toSource p) $ toDamage 3 FromAbility)]
      mark <- putInHand p "33032"
      playOn mark mid []
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainConfused GameRef)]
      choose "Cypher attacks" (cardAbility cypher 301)
      choose "wounded minion" (isTargetChoice mid)
      choose "Cypher overkill response" (cardAbility cypher 1)
      pass
      villainDamageNow `shouldReturn` 1
      length <$> handOf p `shouldReturn` 1

  it "Directed Force is limited to one copy on Telekinesis and expires after that upgrade's attack" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      telekinesis <- putIntoPlay p "41033"
      mid <- addMinion p Minions.tombstone
      mark <- putInHand p "33032"
      playOn mark mid []
      first <- putInHand p "41019"
      second <- putInHand p "41019"
      pay <- putInHand p "01089"
      choose "Telekinesis" (cardAbility telekinesis 1)
      payWith [pay]
      choose "marked target" (isTargetChoice mid)
      playWith first []
      offers (isPlay second) `shouldReturn` False
      pass
      minionDamageOf mid `shouldReturn` Just 5
      getsGame (HashMap.null . gameEffects) `shouldReturn` True
      resolve [UpgradeMessage (upgradeIdOf telekinesis) ReadiedUpgrade]
      nextPay <- putInHand p "01089"
      choose "Telekinesis again" (cardAbility telekinesis 1)
      payWith [nextPay]
      chooseVillain
      offers (isPlay second) `shouldReturn` False
      pass
      villainDamageNow `shouldReturn` 3

  it "Float Like a Butterfly's Telekinesis bonus expires after the upgrade's attack" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      telekinesis <- putIntoPlay p "41033"
      butterfly <- putIntoPlay p "41017"
      mid <- addMinion p Minions.tombstone
      resolve [MinionMessage mid (MinionConfused GameRef)]
      pay <- putInHand p "01089"
      choose "Telekinesis" (cardAbility telekinesis 1)
      payWith [pay]
      choose "confused target" (isTargetChoice mid)
      choose "Butterfly" (cardAbility butterfly 1)
      pass
      minionDamageOf mid `shouldReturn` Just 4
      getsGame (HashMap.null . gameEffects) `shouldReturn` True
      resolve [UpgradeMessage (upgradeIdOf telekinesis) ReadiedUpgrade]
      nextPay <- putInHand p "01089"
      choose "Telekinesis again" (cardAbility telekinesis 1)
      payWith [nextPay]
      choose "same target" (isTargetChoice mid)
      pass
      minionDamageOf mid `shouldReturn` Just 7

  it "Float Like a Butterfly adds damage only to Melee's initially confused target" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      butterfly <- putIntoPlay p "41017"
      confused <- addMinion p Minions.tombstone
      other <- addMinion p Minions.radioactiveMan
      resolve [MinionMessage confused (MinionConfused GameRef)]
      melee <- putInHand p "05030"
      pay <- putInHand p "01089"
      extra <- putInHand p "01091"
      playWith melee [pay,extra]
      choose "confused target" (isTargetChoice confused)
      choose "Butterfly" (cardAbility butterfly 1)
      choose "unconfused target" (isTargetChoice other)
      offers (cardAbility butterfly 1) `shouldReturn` False
      pass
      minionDamageOf confused `shouldReturn` Just 4
      minionDamageOf other `shouldReturn` Just 3

  it "Float Like a Butterfly never changes a different target's simultaneous damage" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      butterfly <- putIntoPlay p "41017"
      other <- addMinion p Minions.tombstone
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainConfused GameRef)]
      let damage = toDamage 3 (FromPlayerAttack p)
      runNow
        [CheckWindows
          [W.Window W.When $ W.IdentityAttackTarget p (EnemyVillainId vid)
          ,W.Window W.When $ W.IdentityAttackTarget p (EnemyMinionId other)]
        ,Simultaneously
          [VillainMessage vid (VillainDamaged (toSource p) damage)
          ,MinionMessage other (MinionDamaged (toSource p) damage)]
        ,CheckWindows [W.Window W.After $ W.PlayerAttackResolved (toSource p) p [EnemyVillainId vid, EnemyMinionId other]]]
      choose "Butterfly" (cardAbility butterfly 1)
      pass
      villainDamageNow `shouldReturn` 4
      minionDamageOf other `shouldReturn` Just 3

  it "Directed Force remains limited and expires when an attacking upgrade was discarded as its cost" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let p = player1 tg
      asHero p
      grenade <- putIntoPlay p "17023"
      mid <- addMinion p Minions.tombstone
      mark <- putInHand p "33032"
      playOn mark mid []
      first <- putInHand p "41019"
      second <- putInHand p "41019"
      _ <- stackEncounterDeck [Treacheries.stampede, Treacheries.stampede]
      choose "Pulse Grenade" (cardAbility grenade 1)
      choose "marked target" (isTargetChoice mid)
      playWith first []
      offers (isPlay second) `shouldReturn` False
      pass
      minionDamageOf mid `shouldReturn` Just 4
      getsGame (HashMap.null . gameEffects) `shouldReturn` True
