module Marvel.Aspect.HerculesPackSpec (spec) where

import Marvel.Ally.Runner qualified as Ally
import Marvel.Minion.Cards qualified as Minions
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Matchers
import Marvel.Query
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero you = changeForm you >> pass >> clearHand you

spec :: Spec
spec = describe "Hercules aspect cards" do
  it "Namora gains one hit point for each other controlled ally" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      _ <- putIntoPlay you "59019"
      namora <- allyIn "59019"
      Ally.getModifiedHitPoints namora `shouldReturn` 4
      _ <- putIntoPlay you "59008"
      Ally.getModifiedHitPoints namora `shouldReturn` 5

  it "Deathcry can pay hero damage to reduce consequential damage for one attack" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      card <- putIntoPlay you "59018"
      deathcry <- allyIn "59018"
      choose "Deathcry attacks" (isAllyAttack deathcry)
      chooseVillain
      choose "Deathcry interrupt" (isAbilityOfCard card)
      pass
      damageOf you `shouldReturn` 1
      allyDamage <$> allyIn "59018" `shouldReturn` 1

  it "Thor thwarts then finds a minion and puts it into play engaged with his controller" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      card <- putIntoPlay you "59020"
      thor <- allyIn "59020"
      _ <- stackEncounterDeck [Minions.hydraRegular]
      placeMainThreat 4
      choose "Thor thwarts" (isAllyThwart thor)
      chooseMainScheme
      choose "Thor forced response" (isAbilityOfCard card)
      pass
      selectAny (MinionIs Minions.hydraRegular <> MinionEngagedWith (IdentityWithId you)) `shouldReturn` True

  it "Gilgamesh confuses a non-Eternal identity as an additional play cost" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      card <- putInHand you "59025"
      payment <- traverse (putInHand you) ["01088", "59008"]
      playWith card payment
      pass
      isConfused you `shouldReturn` True
      ally <- allyIn "59025"
      Ally.getModifiedHitPoints ally `shouldReturn` 6

  it "an already confused non-Eternal identity cannot pay Gilgamesh's confusion cost" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      resolve [IdentityMessage you IdentityConfused]
      card <- putInHand you "59025"
      _ <- traverse (putInHand you) ["01088", "59008"]
      offers (isPlay card) `shouldReturn` False

  it "Recruitment Drive's action makes the next ally free" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      support <- putIntoPlay you "59023"
      clearHand you
      ally <- putInHand you "59019"
      choose "Recruitment Drive" (isAbilityOfCard support)
      whenM (offers $ isTargetChoice you) $ choose "myself" (isTargetChoice you)
      playWith ally []
      pass
      alliesWith "59019" >>= (\cards -> length cards `shouldBe` 1)

  it "Recruitment Drive is discarded after its controller changes to hero form" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      support <- putIntoPlay you "59023"
      changeForm you
      pass
      cards <- discardOf you
      (support `elem` cards) `shouldBe` True

  it "Avenge Me draws two cards when the attached ally is defeated" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      cho <- putIntoPlay you "59008"
      upgrade <- putInHand you "59024"
      playOn upgrade (allyIdOf cho) []
      clearHand you
      runNow [AllyMessage (allyIdOf cho) AllyDefeatedByDamage]
      choose "Avenge Me" (isAbilityOfCard upgrade)
      length <$> handOf you `shouldReturn` 2

  it "Evaluate Threat searches an Avenger and reduces that physical card's cost by two" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      scheme <- putIntoPlay you "59028"
      namora <- putInDiscard you "59018"
      clearHand you
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      runNow [SideSchemeMessage sid DefeatSideScheme]
      choose "search" (isLabel "$label.searchDeck")
      choose "Deathcry" (isTargetChoice namora)
      pass
      payment <- putInHand you "59008"
      playWith namora [payment]
      pass
      alliesWith "59018" >>= (\cards -> length cards `shouldBe` 1)

  it "Ancient Rivalry cannot be played without Thor in play" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      card <- putInHand you "59026"
      _ <- putInHand you "59008"
      offers (isPlay card) `shouldReturn` False

  it "Ancient Rivalry recovers a signature upgrade and readies Hercules and Thor" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      thor <- putIntoPlay you "59020"
      upgrade <- putInDiscard you "59014"
      Lifted.runQueueT (Lifted.capture $ Lifted.exhaust you >> Lifted.exhaust (allyIdOf thor)) >>= resolve
      card <- putInHand you "59026"
      payment <- putInHand you "59008"
      playWith card [payment]
      choose "Golden Mace" (isTargetChoice upgrade)
      pass
      isExhaustedNow you `shouldReturn` False
      allyExhausted <$> allyIn "59020" `shouldReturn` False
      hand <- handOf you
      (upgrade `elem` hand) `shouldBe` True

  it "the Hercules printing of Avengers Compound retains its tuck ability" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      compound <- putIntoPlay you "59032"
      ally <- putInHand you "59019"
      choose "Avengers Compound" (isAbilityOfCard compound)
      choose "tuck Namora" (isTargetChoice ally)
      Lifted.heldCardsAt (supportIdOf compound) `shouldReturn` [ally]
