module Marvel.Hero.WonderManSpec (spec) where

import Data.Aeson.KeyMap qualified as KM
import Marvel.Game.Public (publicGameFor)
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity (currentIdentity)
import Marvel.WonderMan.Helpers
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Matchers
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero you = changeForm you >> pass >> clearHand you

attackValue :: IdentityId -> GameAppT Natural
attackValue you = currentIdentity <$> getPlayer you >>= \case
  HeroSide hero -> Hero.getModifiedAttack $ toAttrs hero
  _ -> pure 0

spec :: Spec
spec = describe "Wonder Man" do
  it "starts Ionic Physiology outside his forty-card deck with twelve hit points" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      hitPointsOf (player1 tg) `shouldReturn` 12
      host <- ionicPhysiologyFor (player1 tg)
      isJust host `shouldBe` True
      ionicCards (player1 tg) `shouldReturn` []

  it "Ionic Physiology raises ATK once for each tucked card" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      attackValue you `shouldReturn` 1
      for_ ["58003", "58004", "58006"] $ \code -> do
        card <- putInDiscard you code
        Lifted.runQueueT (Lifted.capture $ tuckIonicCard you card) >>= resolve
      attackValue you `shouldReturn` 4
      length <$> ionicCards you `shouldReturn` 3

  it "Ionic Physiology cannot accept a fourth tucked card" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      for_ ["58003", "58004", "58005", "58006"] $ \code -> do
        card <- putInDiscard you code
        Lifted.runQueueT (Lifted.capture $ tuckIonicCard you card) >>= resolve
      length <$> ionicCards you `shouldReturn` 3
      cards <- discardOf you
      any ((== "58006") . toCardCode) cards `shouldBe` True

  it "Ionic Blast deals three damage when its cost is paid exactly" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      event <- putInHand you "58004"
      payment <- putInHand you "58003"
      before <- villainDamageNow
      playWith event [payment]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` (before + 3)

  it "Starstruck uses Wonder Man's current ATK" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      card <- putInDiscard you "58003"
      Lifted.runQueueT (Lifted.capture $ tuckIonicCard you card) >>= resolve
      event <- putInHand you "58005"
      payment <- putInHand you "01090"
      before <- villainDamageNow
      playWith event [payment]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` (before + 2)

  it "Wonder Fans can draw a card in alter ego" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      fans <- putIntoPlay you "58007"
      clearHand you
      choose "Wonder Fans" (isAbilityOfCard fans)
      choose "draw" (isLabel "$label.draw1Card")
      length <$> handOf you `shouldReturn` 1

  it "Active Altruism removes one threat without overpaying" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      placeMainThreat 6
      before <- mainSchemeThreatNow
      card <- putInHand you "58003"
      playWith card []
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` (before - 1)

  it "Active Altruism converts two overpaid energy into four additional threat removed" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      placeMainThreat 6
      before <- mainSchemeThreatNow
      card <- putInHand you "58003"
      energy <- putInHand you "01088"
      playWith card [energy]
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` (before - 5)

  it "Ionic Blast adds two damage for an overpaid energy" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      card <- putInHand you "58004"
      energy <- putInHand you "01088"
      before <- villainDamageNow
      playWith card [energy]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` (before + 5)

  it "Starstruck stuns the enemy when overpaid with energy" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      card <- putInHand you "58005"
      energy <- putInHand you "01088"
      playWith card [energy]
      chooseVillain
      pass
      villain <- villainIdNow
      enemyMatches StunnedEnemy (EnemyVillainId villain) `shouldReturn` True

  it "the corrected Ionic Physiology response can tuck at full health" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      placeMainThreat 3
      card <- putInHand you "58003"
      playWith card []
      chooseMainScheme
      discard <- discardOf you
      (card `elem` discard) `shouldBe` True
      host <- ionicPhysiologyFor you
      host `shouldSatisfy` isJust
      for_ host $ \uid -> isBlanked uid `shouldReturn` False
      for_ host $ \uid -> choose "Ionic Physiology" (isAbilityOf uid)
      choose "tuck the played event" (isTargetChoice card)
      ionicCards you `shouldReturn` [card]
      damageOf you `shouldReturn` 0

  it "Jet Belt generates an energy resource for an event" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      belt <- putIntoPlay you "58008"
      card <- putInHand you "58004"
      choose "play Ionic Blast" (isPlay card)
      choose "Jet Belt resource" (isAbilityOfCard belt)
      whenM (offers isFinishPayment) $ choose "finish" isFinishPayment
      chooseVillain
      pass
      upgradeIsExhausted (upgradeIdOf belt) `shouldReturn` True

  it "Mr Hollywood generates one overpayment resource per card, then becomes unavailable" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      hollywood <- putIntoPlay you "58009"
      placeMainThreat 5
      card <- putInHand you "58003"
      choose "play Active Altruism" (isPlay card)
      choose "Mr Hollywood" (isAbilityOfCard hollywood)
      offers (isAbilityOfCard hollywood) `shouldReturn` False
      whenM (offers isFinishPayment) $ choose "finish" isFinishPayment
      chooseMainScheme
      pass

  it "Signature Sunglasses can return a tucked resource after a form change" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      sunglasses <- putIntoPlay you "58010"
      card <- putInDiscard you "58006"
      Lifted.runQueueT (Lifted.capture $ tuckIonicCard you card) >>= resolve
      changeForm you
      choose "Sunglasses" (isAbilityOfCard sunglasses)
      choose "return the tucked resource" (isTargetChoice card)
      hand <- handOf you
      (card `elem` hand) `shouldBe` True
      ionicCards you `shouldReturn` []

  it "What Are You replaces defeat with four remaining hit points and alter ego" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      _ <- putIntoPlay you "58011"
      Lifted.runQueueT (Lifted.capture $ Lifted.damageIdentity you GameRef $ toDamage 12 FromAbility) >>= resolve
      damageOf you `shouldReturn` 8
      (.playerIdentitySide) <$> playerAttrs you `shouldReturn` B
      upgrades <- upgradesOf you
      codes <- traverse (\uid -> getsGame $ fmap cdCardCode . (`abilityCardDef` UpgradeRef uid)) upgrades
      (Just "58011" `elem` codes) `shouldBe` False

  it "Energy Siphon takes one actual damage to supply a second energy resource" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      card <- putInHand you "58004"
      siphon <- putInHand you "58006"
      choose "play Ionic Blast" (isPlay card)
      choose "spend Energy Siphon" (isPayWith siphon)
      choose "take one damage" (isLabelStarting "$label.energySiphonDamage")
      whenM (offers isFinishPayment) $ choose "finish" isFinishPayment
      chooseVillain
      pass
      damageOf you `shouldReturn` 1

  it "Ionic Physiology tucks only the physical event just played, not another discarded copy" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      old <- putInDiscard you "58003"
      placeMainThreat 3
      played <- putInHand you "58003"
      playWith played []
      chooseMainScheme
      host <- ionicPhysiologyFor you
      for_ host $ \uid -> choose "Ionic Physiology" (isAbilityOf uid)
      offers (isTargetChoice old) `shouldReturn` False
      choose "the played copy" (isTargetChoice played)
      ionicCards you `shouldReturn` [played]

  it "tucked Ionic cards are visible to opponents and cannot be played from under Ionic Physiology" do
    tg <- testGame ["wonder-man", "hercules"]
    inGame tg do
      let you = player1 tg
          other = player2 tg
      asHero you
      card <- putInDiscard you "58003"
      Lifted.runQueueT (Lifted.capture $ tuckIonicCard you card) >>= resolve
      offers (isPlay card) `shouldReturn` False
      visible <- liftIO . publicGameFor (== other) =<< getGame
      heldCodesFor "58002" visible `shouldBe` ["58003"]

  it "event attacks do not discard cards tucked under Ionic Physiology" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      tucked <- putInDiscard you "58003"
      Lifted.runQueueT (Lifted.capture $ tuckIonicCard you tucked) >>= resolve
      event <- putInHand you "58004"
      payment <- putInHand you "58003"
      playWith event [payment]
      chooseVillain
      pass
      ionicCards you `shouldReturn` [tucked]

  it "a basic attack discards every tucked Ionic card after resolving the attack" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      tucked <- putInDiscard you "58003"
      Lifted.runQueueT (Lifted.capture $ tuckIonicCard you tucked) >>= resolve
      choose "basic attack" (isHeroAttack you)
      chooseVillain
      choose "Power Recycling" (\case UseAbility ab -> abilitySource ab == toSource you && abilityIndex ab == 1; _ -> False)
      pass
      ionicCards you `shouldReturn` []
      cards <- discardOf you
      (tucked `elem` cards) `shouldBe` True


  it "Energy Siphon cannot pay optional damage through Tough and preserves the status" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      resolve [GiveStatus (IdentityRef you) ToughStatus]
      event <- putInHand you "58004"
      siphon <- putInHand you "58006"
      choose "play Ionic Blast" (isPlay event)
      choose "spend Energy Siphon" (isPayWith siphon)
      offers (isLabelStarting "$label.energySiphonDamage") `shouldReturn` False
      choose "use only the printed energy" (isLabel "$label.noResponse")
      whenM (offers isFinishPayment) $ choose "finish" isFinishPayment
      chooseVillain
      pass
      damageOf you `shouldReturn` 0
      getCharacterStatusCount (IdentityCharacter you) ToughStatus `shouldReturn` 1

  it "Energy Siphon supplied to another player's Alliance card damages only its contributor and pays the enhanced resources" do
    tg <- testGame ["hercules", "wonder-man"]
    inGame tg do
      let payer = player1 tg
          contributor = player2 tg
      asHero payer
      runNow [IdentityMessage contributor $ ChangedToForm A]
      clearHand contributor
      rocket <- putIntoPlay payer "16019"
      event <- putInHand payer "23032"
      _ <- putInHand payer "59008"
      siphon <- putInHand contributor "58006"
      choose "play As One" (isPlay event)
      choose "invite Wonder Man" (isTargetChoice contributor)
      choose "contribute Energy Siphon" (isPayWith siphon)
      choose "take one damage" (isLabelStarting "$label.energySiphonDamage")
      damageOf contributor `shouldReturn` 1
      damageOf payer `shouldReturn` 0
      payWith []
      choose "exhaust Hercules as the Avenger" (isTargetChoice payer)
      choose "exhaust Rocket as the Guardian" (isTargetChoice $ allyIdOf rocket)
      chooseVillain
      pass
      villainDamageNow >>= (\n -> n `shouldSatisfy` (> 0))
      cards <- discardOf contributor
      (siphon `elem` cards) `shouldBe` True

heldCodesFor :: CardCode -> Value -> [CardCode]
heldCodesFor wanted = \case
  Object o ->
    let own = case (KM.lookup "code" o, KM.lookup "heldCards" o) of
          (Just code, Just (Array cards)) | code == toJSON wanted -> foldMap viewCode cards
          _ -> []
     in own <> foldMap (heldCodesFor wanted) (KM.elems o)
  Array values -> foldMap (heldCodesFor wanted) values
  _ -> []

viewCode :: Value -> [CardCode]
viewCode (Object card) = case KM.lookup "code" card of
  Just value -> case fromJSON value of
    Success found -> [found]
    _ -> []
  _ -> []
viewCode _ = []
