module Marvel.Encounter.FalconNemesisSpec (spec) where

import TestImport
import Marvel.Ability qualified as Ability
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Keyword (Keyword(Retaliate))
import Marvel.Modifier
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.Message.Lifted qualified as L

settle :: GameAppT ()
settle = do
  pass
  q <- asked
  case q >>= (headMay . filter forced . snd) of
    Just c -> choose "mandatory" (== c) >> settle
    Nothing -> pure ()
 where
  forced = \case
    UseAbility a -> abilityType a `elem` [Ability.ForcedResponse, Ability.ForcedInterrupt, Ability.WhenRevealed]
    _ -> False

spec :: Spec
spec = describe "Falcon nemesis and Techno" do
  it "Viper has quickstrike and discards five cards after a scheme activation" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.fw53030
      _ <- setEncounterDeck (replicate 8 Cards.fw53032) []
      runNow [MinionMessage mid MinionSchemes]
      settle
      length <$> encounterDiscardNow `shouldReturn` 5

  it "Adder-tisement shuffles Serpent Society minions from discard only" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      _ <- setEncounterDeck [Cards.fw53038] [Cards.fw53030, Cards.fw53032, Cards.fw53039]
      _ <- revealNow p Cards.fw53033
      settle
      codes <- map toCardCode <$> encounterDiscardNow
      codes `shouldSatisfy` notElem "53030"
      codes `shouldSatisfy` notElem "53032"
      codes `shouldSatisfy` elem "53039"

  it "Harlem's Protector stays in play with three counters until its owner pays three times" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      card <- revealNow p Cards.fw53029
      settle
      let oid = ObligationId $ unCardId $ ecCardId card
      getCountersOn (ObligationRef oid) `shouldReturn` 3
      for_ [2,1,0 :: Natural] $ \remaining -> do
        resource <- putInHand p "01086"
        choose "pay emergency" (\case UseAbility a -> abilitySource a == toSource (ObligationRef oid) && abilityIndex a == 1; _ -> False)
        payWith [resource]
        settle
        getCountersOn (ObligationRef oid) `shouldReturn` remaining
      obligationIsInPlay oid `shouldReturn` False

  it "Jet Pack attaches to Fixer ahead of the villain" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.fw53038
      card <- revealNow p Cards.fw53039
      settle
      selectAny (AttachmentWithId (AttachmentId $ unCardId $ ecCardId card) <> AttachmentOnEnemy (EnemyMinionId mid)) `shouldReturn` True

  it "Jet Pack prevents all of a hit of at least three then is discarded" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.fw53038
      card <- revealNow p Cards.fw53039
      settle
      L.runQueueT (L.capture $ L.dealDamage mid GameRef $ toDamage 4 FromAbility) >>= runNow
      settle
      getEnemyRemainingHitPoints (EnemyMinionId mid) `shouldReturn` 15
      selectAny (AttachmentWithId $ AttachmentId $ unCardId $ ecCardId card) `shouldReturn` False

  it "Jet Pack does not prevent a hit below three" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.fw53038
      card <- revealNow p Cards.fw53039
      settle
      L.runQueueT (L.capture $ L.dealDamage mid GameRef $ toDamage 2 FromAbility) >>= runNow
      settle
      getEnemyRemainingHitPoints (EnemyMinionId mid) `shouldReturn` 13
      selectAny (AttachmentWithId $ AttachmentId $ unCardId $ ecCardId card) `shouldReturn` True

  it "Serpent Solutions deals a discarded Serpent Society minion to the first player" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Cards.fw53031
      settle
      cards <- stackEncounterDeck [Cards.fw53032]
      L.runQueueT (L.capture $ L.discardTopOfEncounterDeck 1) >>= runNow
      settle
      dealtTo p >>= (`shouldSatisfy` elem (testHead cards))
      encounterDiscardNow >>= (`shouldSatisfy` notElem (testHead cards))

  it "Tech-Pac adds its printed stats and another attack for each other Tech attachment" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.fw53038
      _ <- revealNow p Cards.fw53040
      settle
      getEnemyScheme (EnemyMinionId mid) `shouldReturn` 3
      _ <- revealNow p Cards.fw53039
      settle
      getModifiers (MinionRef mid) >>= (`shouldSatisfy` elem (AttackModifier 2))

  it "Technological Innovation reveals a Tech attachment before counting additional threat" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      _ <- setEncounterDeck [Cards.fw53039, Minions.hydraSoldier, Minions.hydraSoldier] []
      card <- revealNow p Cards.fw53041
      settle
      sideSchemeThreat (sideSchemeOf card) `shouldReturn` 5
      selectAny (AttachmentIs Cards.fw53039) `shouldReturn` True

  it "Techno activates Fixer already in play against the revealing player" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      _ <- addMinion p Cards.fw53038
      _ <- setEncounterDeck [Minions.hydraSoldier, Minions.hydraSoldier, Minions.hydraSoldier] []
      _ <- revealNow p Cards.fw53042
      settle
      n <- mainSchemeThreatNow
      n `shouldBe` 3

  it "Tech-Pac requires combined ready-character THW strictly greater than its host's SCH" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      changeForm p
      settle
      clearHand p
      _ <- addMinion p Cards.fw53038
      aero <- putIntoPlay p "53015"
      card <- revealNow p Cards.fw53040
      settle
      let aid = AttachmentId $ unCardId $ ecCardId card
          action = \case UseAbility a -> abilitySource a == toSource aid && abilityIndex a == 1; _ -> False
      offers action `shouldReturn` False
      mockingbird <- putIntoPlay p "01083"
      offers action `shouldReturn` True
      choose "remove Tech-Pac" action
      choose "Falcon's THW" (isTargetChoice p)
      choose "Aero's THW" (isTargetChoice $ allyIdOf aero)
      offers (isLabel "$label.done") `shouldReturn` False
      choose "Mockingbird's THW" (isTargetChoice $ allyIdOf mockingbird)
      choose "finish exhaustion" (isLabel "$label.done")
      settle
      selectAny (AttachmentWithId aid) `shouldReturn` False
      isExhaustedNow p `shouldReturn` True

  it "Fixer redirects a modular Tech attachment intended for the villain" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.fw53038
      card <- revealNow p Attachments.smKineticArmor
      settle
      let aid = AttachmentId $ unCardId $ ecCardId card
      selectAny (AttachmentWithId aid <> AttachmentOnEnemy (EnemyMinionId mid)) `shouldReturn` True
      getEnemyKeywords (EnemyMinionId mid) >>= (`shouldSatisfy` elem (Retaliate 1))

  it "Fixer does not redirect a scenario-specific Tech attachment" do
    tg <- testGameAgainst "01134" ["falcon"]
    inGame tg do
      let p = player1 tg
      _ <- addMinion p Cards.fw53038
      card <- revealNow p Attachments.programTransmitter
      settle
      v <- villainIdNow
      selectAny (AttachmentWithId (AttachmentId $ unCardId $ ecCardId card) <> AttachmentOnEnemy (EnemyVillainId v)) `shouldReturn` True
