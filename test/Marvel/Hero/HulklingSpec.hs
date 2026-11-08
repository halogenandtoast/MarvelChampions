module Marvel.Hero.HulklingSpec (spec) where

import Marvel.CampaignPlayerTest
import Marvel.Minion.Cards qualified as Minions
import Marvel.Effect.Types qualified as Effect
import Marvel.Effect.Lasting (Lasting (..))
import Marvel.Matchers qualified as Matchers
import Marvel.Modifier (Modifier (CannotBeHealedByPlayerCards))
import Marvel.Status
import TestImport

spec :: Spec
spec = describe "Hulkling" do
  it "starts with thirteen hit points and five-card alter-ego hand size" do
    tg <- testGame ["hulkling"]
    inGame tg do
      hitPointsOf (player1 tg) `shouldReturn` 13
      handSizeOf (player1 tg) `shouldReturn` 5

  it "has one in each printed hero basic power" do
    tg <- testGame ["hulkling"]
    inGame tg do
      asHero (player1 tg)
      heroPowers (player1 tg) `shouldReturn` (1,1,1)

  for_ [("56032",(2,2,2)),("56033",(1,3,2)),("56034",(1,2,3)),("56035",(3,1,1))] $ \(code,powers) ->
    it ("applies the printed modifiers of Shapeshift " <> show code) do
      tg <- testGame ["hulkling"]
      inGame tg do
        let p = player1 tg
        asHero p
        _ <- putIntoPlay p code
        heroPowers p `shouldReturn` powers

  it "does not offer Chosen Shape for a first shape while Hulkling is already ready" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- paying p "56032" ["01088"]
      offers (isNamedAbility "$label.chosenShape") `shouldReturn` False

  it "still offers Chosen Shape for a first shape when Hulkling can ready" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      asHero p
      resolve [IdentityMessage p ExhaustedIdentity]
      _ <- paying p "56032" ["01088"]
      offers (isNamedAbility "$label.chosenShape") `shouldReturn` True
      choose "Chosen Shape" (isNamedAbility "$label.chosenShape")
      pass
      isExhaustedNow p `shouldReturn` False

  it "still discards an older shape when Hulkling is already ready" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      asHero p
      first <- putIntoPlay p "56032"
      _ <- paying p "56033" ["01088"]
      offers (isNamedAbility "$label.chosenShape") `shouldReturn` True
      choose "Chosen Shape" (isNamedAbility "$label.chosenShape")
      pass
      discardCodes p >>= (`shouldSatisfy` elem "56032")
      upgradesOf p >>= (`shouldSatisfy` notElem (upgradeOf first))

  it "entering a new shape discards the previous shape and readies Hulkling" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      asHero p
      first <- putIntoPlay p "56032"
      resolve [IdentityMessage p ExhaustedIdentity]
      _ <- paying p "56033" ["01088"]
      whenM (offers $ isNamedAbility "$label.chosenShape") $
        choose "Chosen Shape" (isNamedAbility "$label.chosenShape")
      pass
      isExhaustedNow p `shouldReturn` False
      upgradesOf p >>= (`shouldSatisfy` notElem (upgradeOf first))
      discardCodes p >>= (`shouldSatisfy` elem "56032")

  it "Shapeshifter Strike deals five damage" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- paying p "56037" ["01088"]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 5

  it "Altman Residence heals two damage in alter-ego form" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      clearHand p
      setDamage p 4
      residence <- paying p "56031" ["01088"]
      choose "Altman Residence" (cardAbility residence)
      pass
      damageOf p `shouldReturn` 2

  it "Teddy cannot play a second Shapeshift upgrade" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      clearHand p
      _ <- putIntoPlay p "56032"
      second <- putInHand p "56033"
      _ <- putInHand p "01088"
      offers (isPlay second) `shouldReturn` False

  it "Winged Shape removes two threat after an event is played" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      asHero p
      shape <- putIntoPlay p "56032"
      placeMainThreat 5
      before <- mainSchemeThreatNow
      _ <- paying p "01087" ["01088"]
      chooseVillain
      choose "Winged Shape" (cardAbility shape)
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 2
      upgradeIsExhausted (upgradeOf shape) `shouldReturn` True

  it "Hulk Shape deals two ability damage after an event" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      asHero p
      shape <- putIntoPlay p "56033"
      _ <- paying p "01087" ["01088"]
      chooseVillain
      choose "Hulk Shape" (cardAbility shape)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 5

  it "Armored Shape heals two damage after an event" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      asHero p
      shape <- putIntoPlay p "56034"
      setDamage p 4
      _ <- paying p "01087" ["01088"]
      chooseVillain
      choose "Armored Shape" (cardAbility shape)
      pass
      damageOf p `shouldReturn` 2

  it "Armored Shape is not offered when Hulkling has no damage" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      asHero p
      shape <- putIntoPlay p "56034"
      _ <- paying p "01087" ["01088"]
      chooseVillain
      damageOf p `shouldReturn` 0
      offers (cardAbility shape) `shouldReturn` False

  it "Armored Shape is not offered when a card effect prevents healing" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      asHero p
      shape <- putIntoPlay p "56034"
      setDamage p 4
      eid <- getRandom
      withGame_ $ entitiesL . effectsL . at eid ?~ Effect.Effect (Lasting $ Effect.EffectAttrs
        eid "engine-lasting" GameRef (Matchers.IdentityEntity $ Matchers.IdentityWithId p) [CannotBeHealedByPlayerCards] Nothing)
      _ <- paying p "01087" ["01088"]
      chooseVillain
      offers (cardAbility shape) `shouldReturn` False
      damageOf p `shouldReturn` 4

  it "Imitation Shape draws one card after an event" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      asHero p
      shape <- putIntoPlay p "56035"
      _ <- paying p "01087" ["01088"]
      chooseVillain
      before <- length <$> handOf p
      choose "Imitation Shape" (cardAbility shape)
      pass
      (length <$> handOf p) `shouldReturn` before + 1

  it "Impersonation may change to alter-ego after clearing its scheme" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      asHero p
      before <- mainSchemeThreatNow
      when (before < 4) $ placeMainThreat $ 4 - before
      _ <- paying p "56039" ["01088"]
      chooseMainScheme
      choose "change to Teddy" (isLabel "$label.changeToB")
      pass
      (playerIdentitySide . toAttrs <$> getPlayer p) `shouldReturn` B

  it "Shapeshifter searches for a Shapeshift upgrade" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      asHero p
      [shape] <- stackDeck p ["56032"]
      _ <- paying p "56040" []
      choose "Winged Shape" (isTargetChoice shape)
      pass
      map toCardCode <$> handOf p >>= (`shouldSatisfy` elem "56032")

  it "Armored Defense still gives tough after defeating the attacker" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      asHero p
      mid <- addMinion p Minions.hydraBomber
      setDamage p 0
      _ <- putIntoPlay p "56034"
      event <- putInHand p "56038"
      payment <- putInHand p "01090"
      start [MinionMessage mid $ MinionAttacks p]
      playWith event [payment]
      pass
      minionDamageOf mid `shouldReturn` Nothing
      characterHasCondition ToughStatus (IdentityCharacter p) `shouldReturn` True

  it "Wiccan finds an identity-specific event after being played from hand" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      asHero p
      [event] <- stackDeck p ["56037"]
      wiccan <- paying p "56030" ["01088"]
      choose "Wiccan" (cardAbility wiccan)
      choose "Shapeshifter Strike" (isTargetChoice event)
      pass
      map pcCardId <$> handOf p >>= (`shouldSatisfy` elem (pcCardId event))

  it "Alien Physiology generates two resources for a Hulkling card" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      asHero p
      physiology <- putIntoPlay p "56036"
      event <- putInHand p "56037"
      playWith event []
      choose "Alien Physiology" (cardAbility physiology)
      whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 5
      upgradeIsExhausted (upgradeOf physiology) `shouldReturn` True

  it "Alien Physiology produces only one resource for a basic event" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "56036"
      event <- putInHand p "01087"
      offers (isPlay event) `shouldReturn` False

  it "Impersonation does not flip when a replacement prevents clearing the scheme" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "60024"
      main <- mainSchemeIdNow
      overwatch <- putInHand p "30019"
      playOn overwatch main []
      before <- mainSchemeThreatNow
      when (before < 4) $ placeMainThreat $ 4 - before
      _ <- paying p "56039" ["01088"]
      chooseMainScheme
      offers (cardAbility overwatch) `shouldReturn` True
      reducePendingThwart (MainSchemeRef main) 3
      pass
      mainSchemeThreatNow `shouldReturn` 3
      (playerIdentitySide . toAttrs <$> getPlayer p) `shouldReturn` A
      offers (isLabel "$label.changeToB") `shouldReturn` False
