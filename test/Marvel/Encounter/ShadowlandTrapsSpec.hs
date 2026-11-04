module Marvel.Encounter.ShadowlandTrapsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.FearNoEvil.Cards qualified as FNE
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Query
import Marvel.Shadowland.Cards qualified as Cards
import TestImport

spec :: Spec
spec = describe "Shadowland trap choices" do
  for_ [(Cards.street65050,"01060",False),(Cards.street65051,"01003",False)
       ,(Cards.street65052,"01002",False),(Cards.street65053,"01008",False)
       ,(Cards.street65054,"01088",True),(Cards.street65055,"01089",True)
       ,(Cards.street65056,"01005",False)] $ \(def,paymentCode,resources) ->
    it ("pays the printed Avoid cost for " <> show (toCardCode def) <> " without resolving its penalties") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        clearHand you
        payment <- putInHand you paymentCode
        (deck,_) <- setEncounterDeck (replicate 3 FNE.street60188) []
        _ <- revealNow you def
        pass
        choose "avoid the trap" (isLabel "$label.slAvoid")
        if resources then payWith [payment] else chooseIf (isTargetChoice payment)
        pass
        damageOf you `shouldReturn` 0
        mainSchemeThreatNow `shouldReturn` 0
        getEncounterDeck `shouldReturn` deck
        discardOf you >>= \cards -> liftIO $ map pcCardId cards `shouldContain` [pcCardId payment]

  for_ [(Cards.street65050,2,0),(Cards.street65051,2,0),(Cards.street65052,3,3)
       ,(Cards.street65053,0,8),(Cards.street65054,3,0),(Cards.street65055,0,0)
       ,(Cards.street65056,0,0)] $ \(def,damage,discardedCount) ->
    it ("resolves the printed Trip effect for " <> show (toCardCode def)) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        clearHand you
        spare <- putInHand you "01089"
        top <- stackDeck you $ replicate 8 "01088"
        ([_,minion,scheme,_,_,_],_) <- setEncounterDeck [FNE.street60085,FNE.street60092,FNE.street60094,FNE.street60188,FNE.street60188,FNE.street60188] []
        _ <- revealNow you def
        pass
        choose "trip the trap" (isLabel "$label.slTrip")
        when (toCardCode def == "65053") $ chooseIf (isTargetChoice spare)
        pass
        damageOf you `shouldReturn` damage
        discarded <- discardOf you
        let expected = take discardedCount top <> [spare | toCardCode def == "65053"]
        liftIO $ map pcCardId discarded `shouldMatchList` map pcCardId expected
        case toCardCode def of
          "65050" -> getCharacterStatusCount (IdentityCharacter you) ConfusedStatus `shouldReturn` 1
          "65051" -> getCharacterStatusCount (IdentityCharacter you) StunnedStatus `shouldReturn` 1
          "65054" -> isExhaustedPlayer you `shouldReturn` True
          "65055" -> selectAny (MinionWithId $ MinionId $ unCardId $ ecCardId minion) `shouldReturn` True
          "65056" -> selectAny (SideSchemeWithId $ SideSchemeId $ unCardId $ ecCardId scheme) `shouldReturn` True
          _ -> pure ()

  it "disarms Blinding Powder by discarding two random hand cards and giving the chosen character tough" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      clearHand you
      _ <- putInHand you "01088"
      _ <- putInHand you "01089"
      _ <- revealNow you Cards.street65050
      pass
      choose "disarm Blinding Powder" (isLabel "$label.slDisarm")
      chooseIf (isTargetChoice you)
      pass
      length <$> handOf you `shouldReturn` 0
      getCharacterStatusCount (IdentityCharacter you) ToughStatus `shouldReturn` 1

  it "disarms Caltrops by discarding an identity-specific card and damaging the selected enemy" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      clearHand you
      payment <- putInHand you "01008"
      vid <- villainIdNow
      before <- getVillainHitPoints vid
      _ <- revealNow you Cards.street65051
      pass
      choose "disarm Caltrops" (isLabel "$label.slDisarm")
      chooseIf (isTargetChoice payment)
      choose "damage Rhino" (isTargetChoice vid)
      pass
      getVillainHitPoints vid `shouldReturn` subtractNatural 2 before

  it "disarms Flame Spout by discarding a controlled ally and drawing two cards" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      clearHand you
      ally <- putIntoPlay you "01002"
      _ <- revealNow you Cards.street65052
      pass
      choose "disarm Flame Spout" (isLabel "$label.slDisarm")
      chooseIf (isTargetChoice $ allyIdOf ally)
      pass
      selectAny (AllyWithId $ allyIdOf ally) `shouldReturn` False
      length <$> handOf you `shouldReturn` 2

  it "disarms Oil Slick by discarding a controlled upgrade and removing four main-scheme threat" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      clearHand you
      upgrade <- putIntoPlay you "01008"
      scheme <- selectJust EncounterMainScheme
      resolve =<< L.runQueueT (L.capture $ L.placeThreatOn GameRef scheme 6)
      _ <- revealNow you Cards.street65053
      pass
      choose "disarm Oil Slick" (isLabel "$label.slDisarm")
      chooseIf (isTargetChoice $ upgradeIdOf upgrade)
      chooseIf (isTargetChoice scheme)
      pass
      selectAny (UpgradeWithId $ upgradeIdOf upgrade) `shouldReturn` False
      mainSchemeThreatNow `shouldReturn` 2

  it "disarms Poison Darts for two mental and one physical resource and damages the villain" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      clearHand you
      genius <- putInHand you "01089"
      backflip <- putInHand you "01003"
      vid <- villainIdNow
      before <- getVillainHitPoints vid
      _ <- revealNow you Cards.street65054
      pass
      choose "disarm Poison Darts" (isLabel "$label.slDisarm")
      payWith [genius,backflip]
      pass
      getVillainHitPoints vid `shouldReturn` subtractNatural 3 before

  it "disarms Pressure Plate for one energy and two physical resources and searches the top four player cards" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      clearHand you
      energy <- putInHand you "01002"
      strength <- putInHand you "01090"
      [selected,_,_,_] <- stackDeck you ["01089","01088","01090","01003"]
      _ <- revealNow you Cards.street65055
      pass
      choose "disarm Pressure Plate" (isLabel "$label.slDisarm")
      payWith [energy,strength]
      choose "take the selected top-deck card" (isTargetChoice selected)
      pass
      handOf you >>= \cards -> liftIO $ map pcCardId cards `shouldMatchList` [pcCardId selected]

  it "disarms Tripwire by discarding a controlled support and removing three threat from the selected scheme" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      clearHand you
      support <- putIntoPlay you "01091"
      scheme <- selectJust EncounterMainScheme
      resolve =<< L.runQueueT (L.capture $ L.placeThreatOn GameRef scheme 4)
      _ <- revealNow you Cards.street65056
      pass
      choose "disarm Tripwire" (isLabel "$label.slDisarm")
      chooseIf (isTargetChoice $ supportIdOf support)
      chooseIf (isTargetChoice scheme)
      pass
      getsGame (HashMap.member (supportIdOf support) . gameSupports) `shouldReturn` False
      mainSchemeThreatNow `shouldReturn` 1
