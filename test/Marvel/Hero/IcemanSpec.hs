module Marvel.Hero.IcemanSpec (spec) where

import Marvel.Ability qualified as Ability
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers qualified as Ice
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import Marvel.Matchers
import Marvel.Query
import TestImport

asIceman :: IdentityId -> GameAppT ()
asIceman p = changeForm p >> mandatory >> clearHand p

mandatory :: GameAppT ()
mandatory = do
  pass
  cs <- asked
  case cs >>= headMay . filter forced . snd of
    Just c -> choose "mandatory ability" (== c) >> mandatory
    Nothing -> pure ()
 where
  forced = \case
    UseAbility ab -> abilityType ab `elem` [Ability.ForcedInterrupt, Ability.ForcedResponse, Ability.WhenRevealed]
    _ -> False

paying :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
paying p code resources = do
  card <- putInHand p code
  payments <- traverse (putInHand p) resources
  playWith card payments
  pure card

frostbiteCount :: GameAppT Int
frostbiteCount = length <$> upgradesWith "46002"

asideCount :: GameAppT Int
asideCount = length . filter ((== "46002") . toCardCode) <$> getSetAsidePlayerCards

spec :: Spec
spec = describe "Iceman" do
  it "starts with six physical Frostbites set aside, outside the forty-card starter" do
    tg <- testGame ["iceman"]
    inGame tg do
      asideCount `shouldReturn` 6
      frostbiteCount `shouldReturn` 0
      handSizeOf (player1 tg) `shouldReturn` 6
      hitPointsOf (player1 tg) `shouldReturn` 11

  it "Freeze attaches a Frostbite when making a basic attack" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      choose "basic attack" (isHeroAttack p)
      chooseVillain
      choose "Freeze" (isNamedAbility "$label.freeze")
      pass
      frostbiteCount `shouldReturn` 1
      asideCount `shouldReturn` 5
      villainDamageNow `shouldReturn` 2

  it "Frostbites stack attack and scheme penalties, then return after activation" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      v <- villainIdNow
      L.runQueueT (L.capture $ Ice.attachFrostbite p $ EnemyVillainId v) >>= runNow
      pass
      L.runQueueT (L.capture $ Ice.attachFrostbite p $ EnemyVillainId v) >>= runNow
      pass
      getEnemyScheme (EnemyVillainId v) `shouldReturn` 0
      frostbiteCount `shouldReturn` 2
      runNow [VillainMessage v (VillainSchemesAgainst p)]
      mandatory
      frostbiteCount `shouldReturn` 0
      asideCount `shouldReturn` 6

  it "does not remove Frostbite when a stun replaces an attack" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      v <- villainIdNow
      resolve [VillainMessage v (VillainStunned GameRef)]
      L.runQueueT (L.capture $ Ice.attachFrostbite p $ EnemyVillainId v) >>= runNow
      pass
      runNow [VillainMessage v (VillainAttacks p)]
      mandatory
      frostbiteCount `shouldReturn` 1

  it "returns Frostbite to its supply when a minion leaves play" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      mid <- addMinion p Minions.armoredGuard
      L.runQueueT (L.capture $ Ice.attachFrostbite p $ EnemyMinionId mid) >>= runNow
      pass
      resolve [RemoveFromPlay (MinionRef mid)]
      frostbiteCount `shouldReturn` 0
      asideCount `shouldReturn` 6

  it "Cool Off shuffles one Ice card per Frostbite in play and excludes other traits" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      v <- villainIdNow
      L.runQueueT (L.capture $ Ice.attachFrostbite p $ EnemyVillainId v) >>= runNow
      pass
      ice <- putInDiscard p "46009"
      ordinary <- putInDiscard p "46004"
      runNow [IdentityMessage p (ChangedToForm B)]
      choose "Cool Off" (isNamedAbility "$label.coolOff")
      offers (isTargetChoice ordinary) `shouldReturn` False
      choose "recover Ice card" (isTargetChoice ice)
      pass
      cards <- discardOf p
      ice `elem` cards `shouldBe` False
      ordinary `elem` cards `shouldBe` True

  it "Power Belt grants three hit points" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      _ <- paying p "46004" ["01088"]
      hitPointsOf p `shouldReturn` 14

  it "Cryokinetic Perception draws after Freeze and readies only for an Ice card" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      perception <- putIntoPlay p "46005"
      _ <- stackDeck p ["46009"]
      choose "basic attack" (isHeroAttack p)
      chooseVillain
      choose "Freeze" (isNamedAbility "$label.freeze")
      choose "perception" (isAbilityOfCard perception)
      pass
      isExhaustedNow p `shouldReturn` False
      length <$> handOf p `shouldReturn` 1
      upgradeIsExhausted (upgradeOf perception) `shouldReturn` True

  it "Cryokinetic Perception does not ready for a non-Ice draw" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      perception <- putIntoPlay p "46005"
      _ <- stackDeck p ["01093"]
      choose "basic attack" (isHeroAttack p)
      chooseVillain
      choose "Freeze" (isNamedAbility "$label.freeze")
      choose "perception" (isAbilityOfCard perception)
      pass
      isExhaustedNow p `shouldReturn` True

  it "Ice Slide raises all three hero powers and shuffles itself after changing to Bobby" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      slide <- putIntoPlay p "46006"
      choose "attack with Ice Slide" (isHeroAttack p)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 3
      runNow [IdentityMessage p (ChangedToForm B)]
      mandatory
      upgradesWith "46006" `shouldReturn` []
      cards <- discardOf p
      slide `elem` cards `shouldBe` False

  it "Frozen Solid cancels an activation and leaves a Frostbite for the next one" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      solid <- putInHand p "46007"
      payments <- traverse (putInHand p) ["01088","01089"]
      v <- villainIdNow
      playOn solid v payments
      runNow [VillainMessage v (VillainAttacks p)]
      mandatory
      damageOf p `shouldReturn` 0
      upgradesWith "46007" `shouldReturn` []
      frostbiteCount `shouldReturn` 1

  it "Arctic Attack's first mode attaches Frostbite even when its damage defeats the minion" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      mid <- addMinion p Minions.hydraRegular
      _ <- paying p "46009" ["01088"]
      choose "first mode" (isLabel "$label.arcticAttackFreeze")
      choose "minion" (isTargetChoice mid)
      mandatory
      selectList AnyMinion `shouldReturn` []
      asideCount `shouldReturn` 6

  it "Arctic Attack's second mode deals six damage to a frostbitten enemy" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      v <- villainIdNow
      L.runQueueT (L.capture $ Ice.attachFrostbite p $ EnemyVillainId v) >>= runNow
      pass
      _ <- paying p "46009" ["01088"]
      choose "second mode" (isLabel "$label.arcticAttackDamage")
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 6

  it "Chill Out removes three threat then attaches Frostbite" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      main <- mainSchemeIdNow
      resolve [MainSchemeMessage main (MainSchemePlaceThreat 5)]
      _ <- paying p "46011" ["01088"]
      chooseMainScheme
      chooseVillain
      pass
      mainSchemeThreatNow `shouldReturn` 2
      frostbiteCount `shouldReturn` 1

  it "Ice Blast attaches to the chosen player's enemies and deals three per enemy, not per Frostbite" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      _ <- paying p "46010" ["01088","01089"]
      choose "choose player" (isTargetChoice p)
      pass
      villainDamageNow `shouldReturn` 3
      frostbiteCount `shouldReturn` 1

  it "Snow Clone has no upgrade targets" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      _ <- putIntoPlay p "46003"
      inspired <- putInHand p "01074"
      _ <- putInHand p "01088"
      offers (isPlay inspired) `shouldReturn` False
      alliesWith "46003" >>= \xs -> length xs `shouldBe` 1

  it "Freeze can attach during a basic defense and immediately lowers the attack" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      mid <- addMinion p Minions.tigerShark
      runNow [MinionMessage mid (MinionAttacks p)]
      choose "Iceman defends" isDefend
      choose "Freeze on defense" (isNamedAbility "$label.freeze")
      mandatory
      -- Tiger Shark's three ATK is reduced to two, which Iceman's DEF absorbs.
      damageOf p `shouldReturn` 0
      frostbiteCount `shouldReturn` 0
      asideCount `shouldReturn` 6

  it "Ice Wall absorbs all attack damage and its breaking Frostbite survives the activation" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      _ <- putIntoPlay p "46008"
      mid <- addMinion p Minions.laufey
      runNow [MinionMessage mid (MinionAttacks p)]
      mandatory
      damageOf p `shouldReturn` 0
      supportsWith "46008" >>= \walls -> map supportUses walls `shouldBe` [4]
      runNow [MinionMessage mid (MinionAttacks p)]
      mandatory
      damageOf p `shouldReturn` 0
      supportsWith "46008" `shouldReturn` []
      frostbiteCount `shouldReturn` 1
      runNow [MinionMessage mid (MinionAttacks p)]
      mandatory
      damageOf p `shouldReturn` 3
      frostbiteCount `shouldReturn` 0

  it "Ice Wall does not absorb nonattack damage" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      _ <- putIntoPlay p "46008"
      L.runQueueT (L.capture $ L.damageIdentity p GameRef $ toDamage 2 FromAbility) >>= runNow
      mandatory
      damageOf p `shouldReturn` 2
      supportsWith "46008" >>= \walls -> map supportUses walls `shouldBe` [0]

  it "Snow Clone takes no consequential damage attacking a frostbitten enemy it defeats" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      clone <- putIntoPlay p "46003"
      mid <- addMinion p Minions.hydraRegular
      L.runQueueT (L.capture $ Ice.attachFrostbite p $ EnemyMinionId mid) >>= runNow
      pass
      choose "Snow Clone attacks" \case UseAbility ab -> abilitySource ab == AllyRef (allyIdOf clone) && abilityIndex ab == 301; _ -> False
      choose "attack minion" (isTargetChoice mid)
      mandatory
      allyDamage <$> allyIn "46003" `shouldReturn` 0

  it "Power Belt's hero resource pays for an Ice card and exhausts the belt" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      belt <- putIntoPlay p "46004"
      event <- putInHand p "46009"
      resource <- putInHand p "01093"
      choose "play Ice event" (isPlay event)
      choose "Power Belt resource" (isAbilityOfCard belt)
      payWith [resource]
      choose "first mode" (isLabel "$label.arcticAttackFreeze")
      chooseVillain
      pass
      upgradeIsExhausted (upgradeOf belt) `shouldReturn` True
      villainDamageNow `shouldReturn` 4

  it "lethal Arctic Attack still attaches Frostbite and triggers Hot-Headed before it returns to supply" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      _ <- revealNow p Cards.hotHeaded
      mandatory
      mid <- addMinion p Minions.hydraRegular
      _ <- paying p "46009" ["01088"]
      choose "first mode" (isLabel "$label.arcticAttackFreeze")
      choose "minion" (isTargetChoice mid)
      mandatory
      damageOf p `shouldReturn` 1
      selectList (MinionWithId mid) `shouldReturn` []
      asideCount `shouldReturn` 6

  it "Ice Blast also damages previously frostbitten enemies engaged with another player" do
    tg <- testGame ["iceman","spider-man"]
    inGame tg do
      let p = player1 tg
          q = testHead $ testTail $ seats tg
      asIceman p
      mid <- addMinion q Minions.hydraRegular
      L.runQueueT (L.capture $ Ice.attachFrostbite p $ EnemyMinionId mid) >>= runNow
      pass
      _ <- paying p "46010" ["01088","01089"]
      choose "choose Iceman's enemies" (isTargetChoice p)
      mandatory
      selectList (MinionWithId mid) `shouldReturn` []
      villainDamageNow `shouldReturn` 3

  it "Frozen Solid remains attached when a stun replaces the enemy's attack" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      v <- villainIdNow
      solid <- putInHand p "46007"
      payments <- traverse (putInHand p) ["01088","01089"]
      playOn solid v payments
      resolve [VillainMessage v (VillainStunned GameRef)]
      runNow [VillainMessage v (VillainAttacks p)]
      mandatory
      length <$> upgradesWith "46007" `shouldReturn` 1
      frostbiteCount `shouldReturn` 0
      damageOf p `shouldReturn` 0

  it "an Ice Wall borrowed by another hero still attaches its breaking Frostbite" do
    tg <- testGame ["iceman", "rogue"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      asIceman p
      runNow [IdentityMessage q $ ChangedToForm A]
      mandatory
      _ <- putIntoPlay q "46008"
      mid <- addMinion q Minions.laufey
      runNow [MinionMessage mid $ MinionAttacks q]
      mandatory
      runNow [MinionMessage mid $ MinionAttacks q]
      mandatory
      damageOf q `shouldReturn` 0
      frostbiteCount `shouldReturn` 1

  it "an Ice Slide under another player's control still improves Iceman" do
    tg <- testGame ["iceman", "rogue"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      asIceman p
      _ <- putIntoPlay q "46006"
      choose "Iceman attacks" (isHeroAttack p)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 3

  it "Ice Wall can reuse Frostbite returned by the just-finished activation when its supply was empty" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      wall <- putIntoPlay p "46008"
      mid <- addMinion p Minions.laufey
      for_ [1..6 :: Int] $ \_ -> do
        L.runQueueT (L.capture $ Ice.attachFrostbite p $ EnemyMinionId mid) >>= runNow
        pass
      asideCount `shouldReturn` 0
      resolve [SupportMessage (supportIdOf wall) $ AddSupportUses 7]
      runNow [MinionMessage mid $ MinionAttacksWithChange p 10 []]
      mandatory
      damageOf p `shouldReturn` 0
      supportsWith "46008" `shouldReturn` []
      frostbiteCount `shouldReturn` 1
      asideCount `shouldReturn` 5
