module Marvel.CivilWar.ScarletTwinsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (withLeader)
import Marvel.EncounterCard (gatherEncounterSet)
import Marvel.EncounterSet qualified as E
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Modifier
import Marvel.Minion.Cards qualified as M
import Test.Hspec qualified as H
import TestImport

ackForced :: GameAppT ()
ackForced = openQuestion >>= \case
  Just (you, QuestionLabel "$label.resolveForced" _ _) -> answerQuestion you 0 >> ackForced
  _ -> pure ()

spec :: Spec
spec = describe "Synthezoid Scarlet Twins" do
  it "gathers five physical cards" do
    cards <- gatherEncounterSet E.CwScarletTwins
    length cards `H.shouldBe` 5

  for_ [(C.cw57060,StunnedStatus),(C.cw57061,ConfusedStatus)] $ \(def,status) -> do
    for_ [False,True] $ \already ->
      it (show (toCardCode def) <> " applies a status or its otherwise effect: already " <> show already) do
        tg <- testGame ["spider-man"]
        inGame tg do
          let you = player1 tg
          when already $ resolve [GiveStatus (IdentityRef you) status]
          _ <- addMinion you def
          getCharacterStatusCount (IdentityCharacter you) status `shouldReturn` 1
          damageOf you `shouldReturn` if already && status == StunnedStatus then 2 else 0
          mainSchemeThreat `shouldReturn` if status == ConfusedStatus then (if already then 3 else 1) else 0

    it (show (toCardCode def) <> " fills a second steady status without its fallback") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        L.runQueueT (L.capture $ L.lastingModifiers GameRef (SourceEntity $ IdentityRef you) [KeywordModifier K.Steady] False) >>= resolve
        resolve [GiveStatus (IdentityRef you) status]
        _ <- addMinion you def
        getCharacterStatusCount (IdentityCharacter you) status `shouldReturn` 2
        damageOf you `shouldReturn` 0
        mainSchemeThreat `shouldReturn` if status == ConfusedStatus then 1 else 0

  it "Speed quickstrikes a hero before his reveal effect" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- addMinion you C.cw57060
      damageOf you `shouldReturn` 2
      getCharacterStatusCount (IdentityCharacter you) StunnedStatus `shouldReturn` 1

  for_ [("01008",1),("01050",2),("01088",0)] $ \(code,cost) ->
    it ("Superspeed discards a random card and uses its printed cost: " <> show code) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        clearHand you
        payment <- putInHand you code
        card <- encounterCard C.cw57062
        resolve [RevealEncounterCard you card]
        handOf you `shouldReturn` []
        discardOf you >>= (`shouldSatisfy` elem payment)
        damageOf you `shouldReturn` cost

  it "Superspeed does no damage with an empty hand" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      clearHand you
      card <- encounterCard C.cw57062
      resolve [RevealEncounterCard you card]
      damageOf you `shouldReturn` 0

  it "Superspeed's boost lets the player choose and does not inflict indirect damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      own <- withLeader "57040"
      clearHand you
      first <- putInHand you "01008"
      second <- putInHand you "01050"
      _ <- stackEncounterDeck [C.cw57062,M.hydraSoldier]
      start [VillainMessage own VillainSchemes]
      choose "discard Hulk" (isTargetChoice second)
      pass
      handOf you `shouldReturn` [first]
      damageOf you `shouldReturn` 0

  for_ [("01008",["01088"]),("01091",["01088","01090"]),("01050",["01088"]),("01087",["01088"])] $ \(code,payments) ->
    it ("Spellcasting cancels the paid card and discards once: " <> show code) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        changeForm you
        clearHand you
        card <- putInHand you code
        resources <- traverse (putInHand you) payments
        obligation <- encounterCard C.cw57063
        resolve [RevealEncounterCard you obligation]
        before <- villainDamageNow
        playWith card resources
        ackForced
        pass
        discard <- discardOf you
        length (filter (== card) discard) `shouldBe` 1
        for_ resources $ \resource -> discard `shouldSatisfy` elem resource
        handOf you `shouldReturn` []
        getsGame (HashMap.member (upgradeIdOf card) . gameUpgrades) `shouldReturn` False
        getsGame (HashMap.member (supportIdOf card) . gameSupports) `shouldReturn` False
        getsGame (HashMap.member (allyIdOf card) . gameAllies) `shouldReturn` False
        villainDamageNow `shouldReturn` before
        getEncounterDiscard >>= (`shouldSatisfy` elem obligation)

  it "Superpowered Siblings makes only its defeater discard a chosen hand card" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let one = player1 tg
          two = player2 tg
      first <- putInHand two "01008"
      second <- putInHand two "01050"
      hand <- handOf one
      card <- encounterCard C.cw57064
      resolve [RevealEncounterCard one card]
      let sid = SideSchemeId $ unCardId $ ecCardId card
      sideSchemeThreat sid `shouldReturn` 5
      messages <- L.runQueueT $ L.capture $ L.removeSchemeThreat two (SchemeSideSchemeId sid) 5
      start messages
      choose "discard Hulk" (isTargetChoice second)
      pass
      handOf one `shouldReturn` hand
      handOf two >>= (`shouldSatisfy` elem first)
      discardOf two >>= (`shouldSatisfy` elem second)

  it "a canceled attack event still offers Gamora's Finesse" do
    tg <- testGame ["gamora"]
    inGame tg do
      let you = player1 tg
      changeForm you
      pass
      clearHand you
      placeMainThreat 2
      event <- putInHand you "01087"
      energy <- putInHand you "01088"
      obligation <- encounterCard C.cw57063
      resolve [RevealEncounterCard you obligation]
      before <- villainDamageNow
      playWith event [energy]
      ackForced
      offers (isNamedAbility "Finesse") `shouldReturn` True
      choose "Finesse" (isNamedAbility "Finesse")
      mainSchemeIdNow >>= chooseIf . isTargetChoice
      mainSchemeThreatNow `shouldReturn` 1
      villainDamageNow `shouldReturn` before
