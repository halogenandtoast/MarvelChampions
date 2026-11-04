module Marvel.Encounter.FearNoEvilUnderlingsSpec (spec) where

import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Query
import TestImport

chargeFixture :: GameAppT (VillainId,AttachmentId)
chargeFixture = do
  vid <- villainIdNow
  _ <- setEncounterDeck [Cards.street60079,Cards.street60188,Cards.street60188,Cards.street60188] []
  resolve [ReplaceVillain vid "60077" Advanced]
  (vid,) <$> selectJust (AttachmentIs Cards.street60079)

codeChoice :: CardCode -> Choice -> Bool
codeChoice code = \case CardLabel actual _ -> actual == code; _ -> False

spec :: Spec
spec = describe "Fear No Evil underling behaviors" do
  it "Electro II has seventeen hit points and stage III adds three charges and an amplify icon" do
    tg <- testGame ["spider-man"]
    inGame tg do
      (vid,charge) <- chargeFixture
      getVillainHitPoints vid `shouldReturn` 17
      getCountersOn charge `shouldReturn` 2
      resolve [ReplaceVillain vid "60078" Advanced]
      getVillainHitPoints vid `shouldReturn` 20
      getCountersOn charge `shouldReturn` 5
      getAmplifyCount `shouldReturn` 1

  it "Drained of Power exhausts upgrades and prevents their readying until its energy cost is paid" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      upgrade <- putIntoPlay you "01009"
      card <- revealNow you Cards.street60080
      pass
      let aid = AttachmentId $ unCardId $ ecCardId card
          uid = upgradeIdOf upgrade
      [web] <- upgradesWith "01009"
      upgradeExhausted web `shouldBe` True
      resolve [UpgradeMessage uid ReadiedUpgrade]
      [stillExhausted] <- upgradesWith "01009"
      upgradeExhausted stillExhausted `shouldBe` True
      getCountersOn aid `shouldReturn` 3
      energy <- putInHand you "01088"
      runNow [UpgradeMessage uid ReadiedUpgrade]
      choose "Drained of Power interrupts readying" (isAbilityOf aid)
      choose "pay the energy resource" (isLabel "$label.spendAnEnergyResource")
      payWith [energy]
      pass
      [readied] <- upgradesWith "01009"
      upgradeExhausted readied `shouldBe` False
      getCountersOn aid `shouldReturn` 2

  it "Whiplash spends a charge and damages the attacking hero by the discarded card's boost icons" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      (_,charge) <- chargeFixture
      mid <- addMinion you Cards.street60081
      _ <- setEncounterDeck [Cards.street60174,Cards.street60188,Cards.street60188] []
      choose "attack Whiplash" (isHeroAttack you)
      choose "target Whiplash" (isTargetChoice mid)
      choose "resolve Whiplash's interrupt" (isAbilityOf mid)
      pass
      getCountersOn charge `shouldReturn` 1
      damageOf you `shouldReturn` 2
      minionDamageOf mid `shouldReturn` Just 2

  it "Charging Up has six fixed threat, amplifies boosts, and adds three charges when defeated" do
    tg <- testGame ["spider-man"]
    inGame tg do
      (_,charge) <- chargeFixture
      _ <- revealNow (player1 tg) Cards.street60082
      pass
      sid <- selectJust $ SideSchemeIs Cards.street60082
      getSchemeThreatAmount (SchemeSideSchemeId sid) `shouldReturn` 6
      getAmplifyCount `shouldReturn` 1
      resolve =<< L.runQueueT (L.capture $ L.removeSchemeThreat GameRef (SchemeSideSchemeId sid) 6)
      getCountersOn charge `shouldReturn` 5
      getAmplifyCount `shouldReturn` 0

  it "Chain Lightning checks the remaining charge again for each chosen character" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      (_,charge) <- chargeFixture
      ally <- putIntoPlay you "01002"
      resolve =<< L.runQueueT (L.capture $ L.removeCounters charge 1)
      _ <- revealNow you Cards.street60083
      choose "resolve the hero first" (isTargetChoice you)
      choose "take charged lightning damage" (isLabel "$label.fneLightningDamage")
      choose "resolve the ally next" (isTargetChoice $ allyIdOf ally)
      choose "take uncharged lightning damage" (isLabel "$label.fneLightningDamage")
      pass
      damageOf you `shouldReturn` 3
      restored <- allyIn "01002"
      allyDamage restored `shouldBe` 1
      getCountersOn charge `shouldReturn` 0

  it "Energy Overload counts energy cards, allowing a different character to be stunned for each card" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      ally <- putIntoPlay you "01002"
      energy <- putInHand you "01088"
      hulk <- putInHand you "01050"
      _ <- revealNow you Cards.street60084
      choose "resolve the double-energy card" (codeChoice "01088")
      choose "stun the hero" (isTargetChoice you)
      choose "resolve the other energy card" (codeChoice "01050")
      choose "stun the ally" (isTargetChoice $ allyIdOf ally)
      pass
      getCharacterStatusCount (IdentityCharacter you) StunnedStatus `shouldReturn` 1
      getCharacterStatusCount (AllyCharacter $ allyIdOf ally) StunnedStatus `shouldReturn` 1
      handOf you >>= \hand -> liftIO $ map pcCardId hand `shouldMatchList` map pcCardId [energy,hulk]

  it "Shocking Revelation discards once per controlled card and counts energy cards rather than resource icons" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      (_,charge) <- chargeFixture
      _ <- putIntoPlay you "01002"
      _ <- putIntoPlay you "01006"
      _ <- putIntoPlay you "01009"
      cards <- stackDeck you ["01088","01089","01090","01050"]
      _ <- revealNow you Cards.street60085
      choose "add the first charge" (isTargetChoice charge)
      choose "add the second charge" (isTargetChoice charge)
      pass
      getCountersOn charge `shouldReturn` 4
      discarded <- discardOf you
      liftIO $ map pcCardId discarded `shouldContain` map pcCardId cards
