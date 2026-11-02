module Marvel.Hero.HerculesSpec (spec) where

import Marvel.Hercules.Helpers
import Marvel.Hercules.Cards qualified as Cards
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity (currentIdentity)
import Marvel.Minion.Cards qualified as Minions
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero you = changeForm you >> pass >> clearHand you

spec :: Spec
spec = describe "Hercules" do
  it "starts with fourteen hit points and a four-card hero hand size" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      hitPointsOf you `shouldReturn` 14
      handSizeOf you `shouldReturn` 6
      asHero you
      handSizeOf you `shouldReturn` 4
      giftCount you `shouldReturn` 0

  it "The Gift of Battle deals five damage" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      event <- putInHand you "59009"
      payment <- traverse (putInHand you) ["01088", "59008"]
      before <- villainDamageNow
      playWith event payment
      chooseVillain
      villainDamageNow `shouldReturn` (before + 5)

  it "Lion of Olympus adds one to each hero basic power" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      _ <- putIntoPlay you "59016"
      currentIdentity <$> getPlayer you >>= \case
        HeroSide hero -> do
          Hero.getModifiedThwart (toAttrs hero) `shouldReturn` 2
          Hero.getModifiedAttack (toAttrs hero) `shouldReturn` 4
          Hero.getModifiedDefense (toAttrs hero) `shouldReturn` 3
        _ -> error "Hercules should be in hero form"

  for_ [("59005", 16), ("59006", 15), ("59007", 15)] $ \(code, hp) ->
    it (show code <> " is a Gift and grants its printed hit-point bonus") do
      tg <- testGame ["hercules"]
      inGame tg do
        let you = player1 tg
        _ <- putIntoPlay you code
        giftCount you `shouldReturn` 1
        hitPointsOf you `shouldReturn` hp

  it "Son of Zeus gains toughness with two Gifts" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      _ <- putIntoPlay you "59005"
      _ <- putIntoPlay you "59006"
      event <- putInHand you "59010"
      payment <- putInHand you "01088"
      playWith event [payment]
      pass
      (.playerIdentityTough) <$> playerAttrs you `shouldReturn` True

  it "Amadeus Cho exhausts to draw a card in either form" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      cho <- putIntoPlay you "59008"
      clearHand you
      choose "Amadeus Cho draws" (isAbilityOfCard cho)
      length <$> handOf you `shouldReturn` 1
      allyExhausted <$> allyIn "59008" `shouldReturn` True

  it "Wisdom of Athena removes four threat" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      placeMainThreat 6
      before <- mainSchemeThreatNow
      card <- putInHand you "59011"
      payment <- traverse (putInHand you) ["01088", "59008"]
      playWith card payment
      chooseMainScheme
      mainSchemeThreatNow `shouldReturn` (before - 4)

  it "a Gift reduces The Gift of Battle's resource cost by one" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      _ <- putIntoPlay you "59005"
      card <- putInHand you "59009"
      payment <- putInHand you "01088"
      playWith card [payment]
      chooseVillain
      villainDamageNow `shouldReturn` 5

  it "three Gifts make Wisdom of Athena free" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      for_ ["59005", "59006", "59007"] $ \code -> void $ putIntoPlay you code
      clearHand you
      placeMainThreat 5
      before <- mainSchemeThreatNow
      card <- putInHand you "59011"
      playWith card []
      chooseMainScheme
      mainSchemeThreatNow `shouldReturn` (before - 4)

  it "Golden Mace adds one ATK per Gift and overkill for its attack" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      _ <- putIntoPlay you "59005"
      mace <- putIntoPlay you "59014"
      choose "Hercules attacks" (isHeroAttack you)
      choose "Golden Mace" (isAbilityOfCard mace)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 4
      upgradeIsExhausted (upgradeIdOf mace) `shouldReturn` True

  it "Golden Mace gives its attack overkill against a two-hit-point minion" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      _ <- putIntoPlay you "59005"
      mace <- putIntoPlay you "59014"
      enemy <- addMinion you Minions.hydraRegular
      choose "Hercules attacks" (isHeroAttack you)
      choose "Golden Mace" (isAbilityOfCard mace)
      choose "attack the minion" (isTargetChoice enemy)
      pass
      minionDamageOf enemy `shouldReturn` Nothing
      villainDamageNow `shouldReturn` 2

  it "the Sword of Peleus gives Hercules's basic attacks piercing" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      _ <- putIntoPlay you "59007"
      villain <- villainIdNow
      resolve [GiveStatus (VillainRef villain) ToughStatus]
      choose "Hercules attacks" (isHeroAttack you)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 3

  it "Gauntlets of Hercules grants retaliate for each effective Gift during a defense" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      _ <- putIntoPlay you "59005"
      gauntlets <- putIntoPlay you "59013"
      villain <- villainIdNow
      runNow [VillainMessage villain $ VillainAttacksWithoutBoost you]
      choose "Hercules defends" isDefend
      choose "Gauntlets of Hercules" (isAbilityOfCard gauntlets)
      pass
      villainDamageNow `shouldReturn` 1
      upgradeIsExhausted (upgradeIdOf gauntlets) `shouldReturn` True

  it "Herc's Helm reduces the villain's attack damage by one without using basic DEF" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      helm <- putIntoPlay you "59015"
      villain <- villainIdNow
      runNow [VillainMessage villain $ VillainAttacksWithoutBoost you]
      choose "Herc's Helm" (isAbilityOfCard helm)
      pass
      damageOf you `shouldReturn` 1
      isExhaustedNow you `shouldReturn` False

  it "Herc's Helm reduces a villain attack originally targeting an ally" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      helm <- putIntoPlay you "59015"
      cho <- putIntoPlay you "59008"
      _ <- setEncounterDeck [Cards.hc59004] [Cards.hc59040]
      villain <- villainIdNow
      runNow [VillainMessage villain $ VillainAttacksAlly you (allyIdOf cho)]
      choose "Herc's Helm protects the ally" (isAbilityOfCard helm)
      pass
      heroDamage <- damageOf you
      allyDamageTaken <- allyDamage <$> allyIn "59008"
      heroDamage + allyDamageTaken `shouldBe` 1
      isExhaustedNow you `shouldReturn` False

  it "Prince of Power heals the excess damage of a basic attack" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      prince <- putIntoPlay you "59017"
      setDamage you 5
      minion <- addMinion you Minions.hydraRegular
      choose "Hercules attacks" (isHeroAttack you)
      choose "Hydra Regular" (isTargetChoice minion)
      choose "Prince of Power" (isAbilityOfCard prince)
      pass
      damageOf you `shouldReturn` 4

  it "Son of Zeus cannot be played for no effect when Hercules is ready and has no Gifts" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      card <- putInHand you "59010"
      _ <- putInHand you "01088"
      offers (isPlay card) `shouldReturn` False

  it "Son of Zeus can ready Hercules without Gifts" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      resolve [IdentityMessage you ExhaustedIdentity]
      card <- putInHand you "59010"
      payment <- putInHand you "01088"
      playWith card [payment]
      pass
      isExhaustedNow you `shouldReturn` False

  it "Olympus generates one wild resource per Gift when paying for a card" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      _ <- putIntoPlay you "59005"
      olympus <- putIntoPlay you "59012"
      clearHand you
      card <- putInHand you "59009"
      physical <- putInHand you "59008"
      choose "play The Gift of Battle" (isPlay card)
      choose "Olympus resource" (isAbilityOfCard olympus)
      choose "pay the second resource" (isPayWith physical)
      whenM (offers isFinishPayment) $ choose "finish" isFinishPayment
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 5

  it "Olympus cannot exhaust to generate zero resources without an effective Gift" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      olympus <- putIntoPlay you "59012"
      card <- putInHand you "59009"
      _ <- traverse (putInHand you) ["01088", "59008"]
      choose "play The Gift of Battle" (isPlay card)
      offers (isAbilityOfCard olympus) `shouldReturn` False
