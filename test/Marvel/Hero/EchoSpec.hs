module Marvel.Hero.EchoSpec (spec) where

import Marvel.CampaignPlayers.Helpers qualified as Pack
import Marvel.CampaignPlayerTest
import Marvel.Message.Lifted qualified as L
import TestImport

spec :: Spec
spec = describe "Echo" do
  it "has nine hit points and equal printed hero powers" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      hitPointsOf p `shouldReturn` 9
      asHero p
      heroPowers p `shouldReturn` (2,2,2)

  it "Watch and Learn tucks the actual basic event after it resolves" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- paying p "01087" ["01088"]
      chooseVillain
      choose "Watch and Learn" (isNamedAbility "$label.watchAndLearn")
      pass
      cards <- Pack.tuckedPlayerCards p
      map pcCardId cards `shouldSatisfy` elem (pcCardId card)
      discardCodes p >>= (`shouldSatisfy` notElem "01087")

  it "Watch and Learn does not tuck an identity-specific event" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- paying p "60041" []
      pass
      offers (isNamedAbility "$label.watchAndLearn") `shouldReturn` False
      Pack.tuckedPlayerCards p `shouldReturn` []

  it "Muscle Memory returns a tucked event to hand" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putInHand p "01087"
      L.runQueueT (L.capture $ Pack.tuckPlayerCard p card) >>= runNow
      memory <- paying p "60047" ["01088"]
      choose "Muscle Memory" (cardAbility memory)
      choose "tucked event" (isTargetChoice card)
      pass
      Pack.tuckedPlayerCards p `shouldReturn` []
      map toCardCode <$> handOf p >>= (`shouldSatisfy` elem "01087")

  it "Photographic Reflexes replays a tucked event at a discount without retucking it" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putInHand p "01087"
      L.runQueueT (L.capture $ Pack.tuckPlayerCard p card) >>= runNow
      reflexes <- putInHand p "60040a"
      choose "replay Haymaker" (isPlay card)
      choose "discard Photographic Reflexes" (isTargetChoice reflexes)
      whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment
      chooseVillain
      offers (isNamedAbility "$label.watchAndLearn") `shouldReturn` False
      pass
      villainDamageNow `shouldReturn` 3
      Pack.tuckedPlayerCards p `shouldReturn` []
      discardCodes p >>= (`shouldSatisfy` elem "60040a")

  it "cannot replay tucked events without Photographic Reflexes in hand" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putInHand p "01087"
      L.runQueueT (L.capture $ Pack.tuckPlayerCard p card) >>= runNow
      offers (isPlay card) `shouldReturn` False

  it "The Rez heals by the highest printed cost tucked under Maya" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      clearHand p
      card <- putInHand p "01087"
      L.runQueueT (L.capture $ Pack.tuckPlayerCard p card) >>= runNow
      setDamage p 4
      rez <- paying p "60042" ["01090"]
      choose "The Rez" (cardAbility rez)
      pass
      damageOf p `shouldReturn` 2

  it "Innate Reflexes raises DEF without raising ATK or THW" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- paying p "60038" ["01090"]
      pass
      heroPowers p `shouldReturn` (2,2,3)

  it "Photographic Reflexes can replay an interrupt in that interrupt's window" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putInHand p "14015"
      L.runQueueT (L.capture $ Pack.tuckPlayerCard p card) >>= runNow
      reflexes <- putInHand p "60040b"
      vid <- villainIdNow
      start [VillainMessage vid $ VillainAttacksWithoutBoost p]
      reachPlayWindow card
      choose "replay the damage interrupt" (isPlay card)
      choose "discard Reflexes" (isTargetChoice reflexes)
      whenM (offers isFinishPayment) $ choose "finish payment" isFinishPayment
      pass
      damageOf p `shouldReturn` 0
      Pack.tuckedPlayerCards p `shouldReturn` []

  it "Reflexes discounts Make the Call's ally payment" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      ally <- putInDiscard p "01068"
      event <- putInHand p "01071"
      L.runQueueT (L.capture $ Pack.tuckPlayerCard p event) >>= runNow
      reflexes <- putInHand p "60040a"
      energy <- putInHand p "01088"
      choose "replay Make the Call" (isPlay event)
      choose "discard Reflexes" (isTargetChoice reflexes)
      choose "the ally" (isTargetChoice ally)
      payWith [energy]
      pass
      (length <$> alliesWith "01068") `shouldReturn` 1

  it "aborting a replay returns the actual Reflexes and restores the tucked event" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      event <- putInHand p "01054"
      L.runQueueT (L.capture $ Pack.tuckPlayerCard p event) >>= runNow
      reflexes <- putInHand p "60040c"
      _ <- putInHand p "01071"
      choose "replay Uppercut" (isPlay event)
      choose "discard Reflexes" (isTargetChoice reflexes)
      active <- getActiveCost
      for_ active $ \acid -> resolve [AbortPayment acid]
      pass
      cards <- Pack.tuckedPlayerCards p
      map pcCardId cards `shouldSatisfy` elem (pcCardId event)
      hand <- handOf p
      map pcCardId hand `shouldSatisfy` elem (pcCardId reflexes)
      map pcCardId hand `shouldSatisfy` notElem (pcCardId event)
      villainDamageNow `shouldReturn` 0

  it "a three-cost replay remains unplayable without one additional resource" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      event <- putInHand p "01054"
      L.runQueueT (L.capture $ Pack.tuckPlayerCard p event) >>= runNow
      _ <- putInHand p "60040a"
      offers (isPlay event) `shouldReturn` False

  it "Choreography shuffles an aspect event and draws while in alter-ego form" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      clearHand p
      discarded <- putInDiscard p "01054"
      choreography <- paying p "60044" ["01090"]
      before <- length <$> handOf p
      choose "Choreography" (cardAbility choreography)
      choose "shuffle Uppercut" (isTargetChoice discarded)
      pass
      (length <$> handOf p) `shouldReturn` before + 1
      discardCodes p >>= (`shouldSatisfy` notElem "01054")

  it "Echo's Katana deals the event's printed cost as piercing damage" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      katana <- putIntoPlay p "60045"
      _ <- paying p "01087" ["01088"]
      chooseVillain
      choose "Echo's Katana" (cardAbility katana)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 5

  it "Improvisation heals after an Attack-trait event" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      improvisation <- putIntoPlay p "60046"
      setDamage p 3
      _ <- paying p "01087" ["01088"]
      chooseVillain
      choose "Improvisation" (cardAbility improvisation)
      pass
      damageOf p `shouldReturn` 2

  it "Maya searches for an aspect or basic event before changing to hero form" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      clearHand p
      [event] <- stackDeck p ["01087"]
      changeForm p
      choose "Practice Makes Perfect" (isNamedAbility "$label.practiceMakesPerfect")
      choose "Haymaker" (isTargetChoice event)
      pass
      map pcCardId <$> handOf p >>= (`shouldSatisfy` elem (pcCardId event))

  it "Study the Tape can take an event from another player's discard pile" do
    tg <- testGame ["echo","spider-man"]
    inGame tg do
      let p = player1 tg
          other = player2 tg
      clearHand p
      event <- putInDiscard other "01087"
      _ <- paying p "60041" []
      choose "take the other player's event" (isTargetChoice event)
      pass
      map pcCardId <$> handOf p >>= (`shouldSatisfy` elem (pcCardId event))
      discardCodes other >>= (`shouldSatisfy` notElem "01087")

  it "Muscle Memory is unavailable without a tucked event" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      asHero p
      memory <- putIntoPlay p "60047"
      offers (cardAbility memory) `shouldReturn` False
